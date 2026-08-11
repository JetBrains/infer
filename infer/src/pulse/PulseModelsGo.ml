(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Models of the [$builtins.go_*] intrinsics the Go frontend (go2sil) emits for slices and maps.

    The scope is the nilability of the collection itself, nothing else: the elements are not
    modelled, and neither are the index bounds. Go's rules for a nil collection are asymmetric,
    which is the whole reason these need models rather than an opaque declaration:

    - [len] and [cap] of nil are 0, not a panic;
    - indexing a nil slice panics, in read *and* in write position;
    - reading a nil map is fine (it yields the zero value, and [ok] is false), writing to one
      panics;
    - [append] on nil allocates, [delete] on nil is a no-op, ranging over nil iterates 0 times.

    Two things follow. First, the panicking operations are reported through the ordinary
    null-dereference machinery ([check_valid]), so they come out manifest on a known-nil collection
    and latent on a parameter, exactly like a plain [*p]. Second, [len] cannot just answer "some
    non-negative number": [if len(s) > 0 { use(s[0]) }] is a legal Go nil guard, so the model has to
    tie the answer to the nilability of its argument, and it does that by returning two disjuncts
    whose path conditions a later [prune] can contradict.

    Every model here stays within two disjuncts. [--pulse-max-disjuncts] defaults to 20 and silently
    drops the excess, so a chattier model would under-approximate loops over collections. *)

open! IStd
open PulseBasicInterface
module DSL = PulseModelsDSL

(* The two opaque collection types go2sil declares. Neither carries an element type: with the
   contents unmodelled, one type serves every slice and one every map. *)
let slice_type_name = Typ.Name.C.from_string "$slice"

let map_type_name = Typ.Name.C.from_string "$map"

(* The length of a collection, kept as a fake field on the collection object rather than conjured
   afresh per call. That is what makes two consecutive [len(s)] agree, and what lets a [len(s) > 0]
   test taken earlier still rule out [s == nil] later on. Same trick as the Java collection models'
   [__infer_model_backing_collection_*] fields; the field needs no declaration in Textual because
   only these models ever touch it. *)
let len_field = Fieldname.make PulseOperations.pulse_model_type "__infer_model_go_len"

let read_len coll =
  let open DSL.Syntax in
  load_access ~deref:false coll (FieldAccess len_field)


let write_len coll len =
  let open DSL.Syntax in
  write_field ~ref:coll len_field len


(* A freshly built collection of the given length: allocated, hence never nil. *)
let fresh_collection type_name len =
  let open DSL.Syntax in
  let* coll = constructor type_name [] in
  let* () = write_len coll len in
  assign_ret coll


(* [make([]T, n)]. Go panics on a negative n, so that path is dropped rather than modelled. *)
let make_slice n =
  let open DSL.Syntax in
  let* () = prune_ge_int n IntLit.zero in
  fresh_collection slice_type_name n


let make_map () =
  let open DSL.Syntax in
  let* zero = int 0 in
  fresh_collection map_type_name zero


(* [len(x)]: 0 on a nil collection, its real length otherwise. Both facts must land in the *same*
   disjunct -- that is what makes [if len(s) > 0 { s[0] }] a nil guard, because the [len > 0] path
   condition is unsatisfiable in the nil disjunct and pulse then drops it. A single fresh
   non-negative result would give no correlation at all. *)
let collection_len coll =
  let open DSL.Syntax in
  disj
    [ (let* () = prune_eq_zero coll in
       let* zero = int 0 in
       assign_ret zero )
    ; (let* () = prune_ne_zero coll in
       let* len = read_len coll in
       let* () = prune_ge_int len IntLit.zero in
       assign_ret len ) ]


(* [cap(s)]: same correlation, but with no field behind it -- nothing downstream needs two [cap]
   calls to agree, and capacity is not the length. *)
let slice_cap slice =
  let open DSL.Syntax in
  disj
    [ (let* () = prune_eq_zero slice in
       let* zero = int 0 in
       assign_ret zero )
    ; (let* () = prune_ne_zero slice in
       assign_ret @= fresh_nonneg () ) ]


(* [&s[i]], and therefore [s[i]] in both read and write position: indexing a nil slice panics
   ("index out of range"). The returned address is fresh, because the contents of a slice are not
   modelled; the bounds are not modelled either, so indexing a non-nil slice is always allowed --
   a documented false negative. The same model serves [&a[i]] on an array pointer, where the panic
   is the ordinary one for dereferencing nil. *)
let index_addr coll_arg =
  let open DSL.Syntax in
  let* () = check_valid coll_arg in
  assign_ret @= fresh ()


(* [m[k]] in value position: legal on a nil map, where Go yields the zero value of the element type.
   The result is left unconstrained, which covers "the zero value may be nil" without claiming it
   is. *)
let map_load () =
  let open DSL.Syntax in
  assign_ret @= fresh ()


(* The [ok] of [v, ok := m[k]]: a nil map holds no entries, so [ok] can only be false there. *)
let map_load_ok map =
  let open DSL.Syntax in
  disj
    [ (let* zero = int 0 in
       assign_ret zero )
    ; (let* () = prune_ne_zero map in
       let* one = int 1 in
       assign_ret one ) ]


(* The [ok] of one [range m] step. Reaching the body means the map is neither nil nor empty, which
   is what keeps [for k := range m { m[k] = f(k) }] free of false positives. *)
let map_next map =
  let open DSL.Syntax in
  disj
    [ (let* zero = int 0 in
       assign_ret zero )
    ; (let* () = prune_ne_zero map in
       let* len = read_len map in
       let* () = prune_gt_int len IntLit.zero in
       let* one = int 1 in
       assign_ret one ) ]


(* [m[k] = v] panics on a nil map ("assignment to entry in nil map"). Afterwards the map holds at
   least that one entry: without refreshing the length here a just-[make]d map would stay at
   [len == 0] for good, and [if len(m) == 0] after a store would become a real false positive.

   No [prune_ne_zero] after the check, tempting though it is: a [m <> 0] conjunct in the
   precondition makes applying this summary to a literal nil argument [Unsat], so the disjunct is
   dropped instead of reported, and [writeTo(nil)] goes silent. Abducing [MustBeValid] is what
   carries the obligation to the caller. *)
let map_store map_arg =
  let open DSL.Syntax in
  let* () = check_valid map_arg in
  let* len = fresh_nonneg () in
  let* () = prune_ge_int len IntLit.one in
  write_len (to_aval map_arg) len


(* [delete(m, k)] is a no-op on a nil map. Elsewhere it can only shrink the map, so all that is left
   of the length is that it is still non-negative. *)
let map_delete map =
  let open DSL.Syntax in
  disj
    [ prune_eq_zero map
    ; (let* () = prune_ne_zero map in
       let* len = fresh_nonneg () in
       write_len map len ) ]


(* [append(s, v...)] with at least one value appended: always a fresh non-nil slice, and a non-empty
   one. [len(append(s, v)) = len(s) + 1] would buy nothing for nilability. [append(s)] with nothing
   to append never reaches here -- the frontend passes [s] straight through, which is also what Go
   does. *)
let append () =
  let open DSL.Syntax in
  let* len = fresh_nonneg () in
  let* () = prune_ge_int len IntLit.one in
  fresh_collection slice_type_name len


(* [a[lo:hi]] where [a] is an array: an array is a value, so the result is never nil. The length
   comes from the frontend, which computes it from the bounds. *)
let slice_from_array array_arg len =
  let open DSL.Syntax in
  let* () = check_valid array_arg in
  let* () = prune_ge_int len IntLit.zero in
  fresh_collection slice_type_name len


(* [s[lo:hi]] where [s] is a slice passes nilability straight through: slicing nil is legal and
   yields nil, slicing a real slice yields a real slice whose length we forget. *)
let slice_slice slice =
  let open DSL.Syntax in
  disj
    [ (let* () = prune_eq_zero slice in
       assign_ret @= null )
    ; (let* () = prune_ne_zero slice in
       let* len = fresh_nonneg () in
       fresh_collection slice_type_name len ) ]


(* [copy(dst, src)] is legal with a nil slice on either side, and returns how many elements moved.
   The frontend passes neither slice, exactly because neither can be wrong. *)
let copy () =
  let open DSL.Syntax in
  assign_ret @= fresh_nonneg ()


let builtins_matcher builtin (func_args : ValueOrigin.t FuncArg.t list) :
    unit -> unit DSL.model_monad =
  let open ProcnameDispatcherBuiltins in
  let builtin_s = GoProcname.show_builtin builtin in
  let args = List.map func_args ~f:(fun fa -> FuncArg.arg_payload fa |> ValueOrigin.addr_hist) in
  match (builtin : GoProcname.builtin) with
  | Append ->
      (* the source slice is passed for readable traces and arity checking only *)
      let _s = expect_1_arg args builtin_s in
      fun () -> append ()
  | Copy ->
      fun () -> copy ()
  | MakeMap ->
      fun () -> make_map ()
  | MakeSlice ->
      let n = expect_1_arg args builtin_s in
      fun () -> make_slice n
  | MapDelete ->
      let m = expect_1_arg args builtin_s in
      fun () -> map_delete m
  | MapLen | SliceLen ->
      let coll = expect_1_arg args builtin_s in
      fun () -> collection_len coll
  | MapLoad ->
      let _m = expect_1_arg args builtin_s in
      fun () -> map_load ()
  | MapLoadOk ->
      let m = expect_1_arg args builtin_s in
      fun () -> map_load_ok m
  | MapNext ->
      let m = expect_1_arg args builtin_s in
      fun () -> map_next m
  | MapStore ->
      let m = expect_1_arg func_args builtin_s in
      fun () -> map_store (FuncArg.arg_payload m)
  | SliceCap ->
      let s = expect_1_arg args builtin_s in
      fun () -> slice_cap s
  | SliceFromArray ->
      let a, n = expect_2_args func_args builtin_s in
      fun () ->
        slice_from_array (FuncArg.arg_payload a)
          (FuncArg.arg_payload n |> ValueOrigin.addr_hist)
  | ArrayIndexAddr | SliceIndexAddr ->
      let s = expect_1_arg func_args builtin_s in
      fun () -> index_addr (FuncArg.arg_payload s)
  | SliceSlice ->
      let s = expect_1_arg args builtin_s in
      fun () -> slice_slice s
