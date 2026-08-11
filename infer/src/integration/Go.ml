(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

(** Capture Go programs.

    The translation itself is done by an external translator (go2sil) which compiles a set of Go
    packages down to Textual SIL and prints one textual unit per [.go] source file on its standard
    output, delimited with the same markers as the hackc/infer protocol:

    {v
    // TEXTUAL UNIT START <absolute path of the .go file>
    ...textual module...
    // TEXTUAL UNIT END <absolute path of the .go file>
    v}

    Units carry [// .line N] / [// .column N] markers that we turn into a {!LineMap}, so that the
    captured procedures point at the original Go sources. Units are captured sequentially in the
    infer process: Go packages are small compared to Hack codebases and this keeps the integration
    trivial (no worker pools, no intermediate databases). *)

let unit_start_marker = "// TEXTUAL UNIT START "

let unit_end_marker = "// TEXTUAL UNIT END "

type unit_ = {source_path: string; content: string}

(** parse the marker-delimited stream; dies on a malformed stream since that is a go2sil bug *)
let extract_units ic =
  let rec loop acc current =
    match (In_channel.input_line ic, current) with
    | None, None ->
        List.rev acc
    | None, Some (source_path, _) ->
        L.die ExternalError "go2sil output ended in the middle of textual unit %s" source_path
    | Some line, None ->
        let stripped = String.strip line in
        ( match String.chop_prefix stripped ~prefix:unit_start_marker with
        | Some source_path ->
            loop acc (Some (String.strip source_path, Buffer.create 4096))
        | None ->
            (* the count marker and anything else outside of units is ignored *)
            loop acc None )
    | Some line, Some ((source_path, buffer) as unit_in_progress) -> (
        let stripped = String.strip line in
        match String.chop_prefix stripped ~prefix:unit_end_marker with
        | Some end_path when String.equal (String.strip end_path) source_path ->
            loop ({source_path; content= Buffer.contents buffer} :: acc) None
        | Some end_path ->
            L.die ExternalError "go2sil unit end marker mismatch: started %s but ended %s"
              source_path (String.strip end_path)
        | None ->
            Buffer.add_string buffer line ;
            Buffer.add_char buffer '\n' ;
            loop acc (Some unit_in_progress) )
  in
  loop [] None


let textual_to_sil file =
  let open IResult.Let_syntax in
  let open TextualParser in
  let* sourcefile, textual_parsed = TextualFile.parse file in
  let* textual_verified = TextualFile.verify sourcefile textual_parsed in
  TextualFile.textual_to_sil sourcefile textual_verified


(** capture one textual unit; returns [true] on success. Errors are logged and turned into a global
    failure at the end of [capture]: a unit that infer rejects must never look like a successful
    "no issues found" run. *)
let capture_unit ~global_tenv {source_path; content} =
  L.debug Capture Quiet "Capturing %s@\n" source_path ;
  let line_map = LineMap.create content in
  let file = TextualParser.TextualFile.TranslatedFile {source_path; content; line_map} in
  match textual_to_sil file with
  | Ok (sil, textual_module) ->
      TextualParser.TextualFile.capture ~textual_module ~use_global_tenv:true sil ;
      Tenv.merge ~src:sil.TextualParser.TextualFile.tenv ~dst:global_tenv ;
      true
  | Error (sourcefile, errs) ->
      List.iter errs ~f:(fun error ->
          L.external_error "%a@\n" (TextualParser.pp_error sourcefile) error ) ;
      false


let run_translator prog args =
  let {IUnix.Process_info.stdin; stdout; stderr; pid} = IUnix.create_process ~prog ~args in
  Unix.close stdin ;
  let stdout_ic = Unix.in_channel_of_descr stdout in
  let stderr_ic = Unix.in_channel_of_descr stderr in
  let units = extract_units stdout_ic in
  let error_output = In_channel.input_all stderr_ic in
  In_channel.close stdout_ic ;
  In_channel.close stderr_ic ;
  ( match IUnix.waitpid pid with
  | Ok () ->
      if not (String.is_empty error_output) then
        L.debug Capture Quiet "%s@\n" error_output
  | Error _ as status ->
      L.die ExternalError "*** %s exited with %s@\n*** %s@." prog
        (IUnix.Exit_or_signal.to_string_hum status)
        error_output ) ;
  units


let capture ~prog ~args =
  let units = run_translator prog args in
  if List.is_empty units then L.die ExternalError "go2sil produced no textual units" ;
  let global_tenv = Tenv.create () in
  let failures =
    List.count units ~f:(fun unit_ -> not (capture_unit ~global_tenv unit_))
  in
  Tenv.Global.store ~normalize:false global_tenv ;
  L.progress "Captured %d Go source file%s@." (List.length units)
    (if Int.equal (List.length units) 1 then "" else "s") ;
  if failures > 0 then
    L.die ExternalError "infer rejected %d of %d textual unit%s produced by go2sil (see above)"
      failures (List.length units)
      (if Int.equal failures 1 then "" else "s")
