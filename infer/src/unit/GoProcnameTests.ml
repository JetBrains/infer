(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
module F = Format

let assert_rendered ~verbosity ~expected procname =
  assert_equal ~printer:Fn.id expected (Procname.to_string ~verbosity procname)


let assert_described ~expected procname =
  assert_equal ~printer:Fn.id expected (F.asprintf "%a" Procname.describe procname)


let test_function_name =
  "function_name"
  >:: fun _ ->
  let procname = Procname.make_go ~name:"go2sil::testdata::errors::basic::open" in
  assert_rendered ~verbosity:Non_verbose ~expected:"basic.open" procname ;
  assert_rendered ~verbosity:Simple ~expected:"open()" procname ;
  assert_rendered ~verbosity:Verbose ~expected:"go2sil::testdata::errors::basic::open" procname ;
  assert_equal "go2sil::testdata::errors::basic::open" (Procname.to_unique_id procname) ;
  assert_described ~expected:"basic.open()" procname ;
  assert_equal "open" (Procname.get_method procname)


let test_standard_library_function_name =
  "standard_library_function_name"
  >:: fun _ ->
  let procname = Procname.make_go ~name:"errors::New" in
  assert_rendered ~verbosity:Non_verbose ~expected:"errors.New" procname


let test_method_name =
  "method_name"
  >:: fun _ ->
  let class_name = Typ.Name.C.from_string "go2sil::testdata::deref::methods::Counter" in
  let procname = Procname.make_go_method ~class_name ~method_name:"get" in
  assert_rendered ~verbosity:Non_verbose ~expected:"methods.Counter.get" procname ;
  assert_rendered ~verbosity:Simple ~expected:"get()" procname ;
  assert_rendered ~verbosity:Verbose ~expected:"go2sil::testdata::deref::methods::Counter_get"
    procname ;
  assert_equal "go2sil::testdata::deref::methods::Counter_get" (Procname.to_unique_id procname) ;
  assert_described ~expected:"methods.Counter.get()" procname


let tests =
  "GoProcnameTests" >::: [test_function_name; test_standard_library_function_name; test_method_name]
