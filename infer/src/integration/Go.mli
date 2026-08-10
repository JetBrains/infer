(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val capture : prog:string -> args:string list -> unit
(** run the go2sil translator with the given arguments and capture the textual units it prints on
    its standard output *)
