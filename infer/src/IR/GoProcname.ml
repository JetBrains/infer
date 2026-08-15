(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
include PpDetailLevel

type builtin =
  | Append
  | ArrayIndexAddr
  | Copy
  | MakeMap
  | MakeSlice
  | MapDelete
  | MapLen
  | MapLoad
  | MapLoadOk
  | MapNext
  | MapStore
  | SliceCap
  | SliceFromArray
  | SliceIndexAddr
  | SliceLen
  | SliceSlice
[@@deriving compare, equal, yojson_of, sexp, hash, normalize, enumerate]

type regular =
  | Function of Typ.c_function_sig
  | Method of {class_name: Typ.Name.t; method_name: string}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

type t = Builtin of builtin | Regular of regular
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let mk_builtin builtin = Builtin builtin

let regular_from_string name =
  Regular
    (Function
       { Typ.c_name= QualifiedCppName.of_qual_string name
       ; c_mangled= None
       ; c_template_args= NoTemplate } )


let method_ ~class_name ~method_name = Regular (Method {class_name; method_name})

let replace_class t class_name =
  match t with Regular (Method {method_name}) -> method_ ~class_name ~method_name | _ -> t


let get_class_type_name = function Regular (Method {class_name}) -> Some class_name | _ -> None

let show_builtin = function
  | Append ->
      "go_append"
  | ArrayIndexAddr ->
      "go_array_index_addr"
  | Copy ->
      "go_copy"
  | MakeMap ->
      "go_make_map"
  | MakeSlice ->
      "go_make_slice"
  | MapDelete ->
      "go_map_delete"
  | MapLen ->
      "go_map_len"
  | MapLoad ->
      "go_map_load"
  | MapLoadOk ->
      "go_map_load_ok"
  | MapNext ->
      "go_map_next"
  | MapStore ->
      "go_map_store"
  | SliceCap ->
      "go_slice_cap"
  | SliceFromArray ->
      "go_slice_from_array"
  | SliceIndexAddr ->
      "go_slice_index_addr"
  | SliceLen ->
      "go_slice_len"
  | SliceSlice ->
      "go_slice_slice"


let builtin_from_string =
  let tbl = IString.Hash.create (List.length all_of_builtin) in
  List.iter all_of_builtin ~f:(fun builtin -> IString.Hash.add tbl (show_builtin builtin) builtin) ;
  fun str -> IString.Hash.find_opt tbl str


let get_method = function
  | Builtin builtin ->
      show_builtin builtin
  | Regular (Function {Typ.c_name}) ->
      QualifiedCppName.to_qual_string c_name
  | Regular (Method {method_name}) ->
      method_name


let regular_to_string = function
  | Function {Typ.c_name} ->
      QualifiedCppName.to_qual_string c_name
  | Method {class_name; method_name} ->
      F.asprintf "%s_%s" (Typ.Name.name class_name) method_name


(* Regular Go procnames must render exactly as [Procname.C] used to render them: go2sil names a
   function [<mangled import path>::<name>], and that string is what shows up in reports. *)
let pp verbosity fmt = function
  | Builtin builtin ->
      F.pp_print_string fmt (show_builtin builtin)
  | Regular regular -> (
      let plain = regular_to_string regular in
      match verbosity with
      | Simple ->
          F.fprintf fmt "%s()" plain
      | Non_verbose | FullNameOnly | NameOnly | Verbose ->
          F.pp_print_string fmt plain )
