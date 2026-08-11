(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** The intrinsics the Go frontend (go2sil) emits instead of the real semantics of an operation on a
    slice or a map. They live in the reserved [$builtins] namespace of a Textual unit, have no body,
    and are executed by [PulseModelsGo] before the regular model dispatcher runs. Only the
    nilability of the collection itself is modelled; the contents are not, which is why no
    intrinsic takes a key, an index or an appended element. *)
type builtin =
  | Append  (** [append(s, v...)]: a fresh non-nil slice; the added values are not modelled. *)
  | ArrayIndexAddr  (** [&a[i]] through an array pointer: panics on nil. *)
  | Copy  (** [copy(dst, src)]: a non-negative count; nil is legal on both sides. *)
  | MakeMap  (** [make(map[K]V)]: a fresh non-nil map of length 0. *)
  | MakeSlice  (** [make([]T, n)]: a fresh non-nil slice of length [n]. *)
  | MapDelete  (** [delete(m, k)]: legal on a nil map, and only ever shrinks it. *)
  | MapLen  (** [len(m)]: 0 on a nil map. *)
  | MapLoad  (** [m[k]]: legal on a nil map, where it yields the zero value. *)
  | MapLoadOk  (** the [ok] of [v, ok := m\[k\]]: never true on a nil map. *)
  | MapNext  (** the [ok] of one [range m] step: never true on a nil or empty map. *)
  | MapStore  (** [m[k] = v]: panics on a nil map. *)
  | SliceCap  (** [cap(s)]: 0 on a nil slice. *)
  | SliceFromArray  (** [a[:]] on an array: a fresh non-nil slice of the given length. *)
  | SliceIndexAddr  (** [&s[i]]: panics on a nil slice. *)
  | SliceLen  (** [len(s)]: 0 on a nil slice. *)
  | SliceSlice  (** [s[lo:hi]] on a slice: nil in, nil out. *)
[@@deriving compare, equal, yojson_of, sexp, hash, normalize, enumerate]

type t = Builtin of builtin | Regular of Typ.c_function_sig
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val mk_builtin : builtin -> t

val regular_from_string : string -> t

val show_builtin : builtin -> string

val builtin_from_string : string -> builtin option
(** the inverse of [show_builtin], for the [$builtins.<name>] calls of a Textual unit *)

val get_method : t -> string

val pp : PpDetailLevel.t -> F.formatter -> t -> unit
