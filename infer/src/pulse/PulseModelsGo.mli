(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val builtins_matcher :
     GoProcname.builtin
  -> PulseBasicInterface.ValueOrigin.t FuncArg.t list
  -> unit
  -> unit PulseModelsDSL.model_monad
