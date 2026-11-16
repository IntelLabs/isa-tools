(****************************************************************
 * Test global variable wrapping transform
 *
 * Copyright (C) 2024-2026 Intel Corporation
 * SPDX-License-Identifier: BSD-3-Clause
 ****************************************************************)

open Test_utils
open LibISA
open Isa_utils
module TC = Tcheck

(****************************************************************
 * Test global variable wrapping transform
 ****************************************************************)

let wrap_tests : unit Alcotest.test_case list =
  TC.enable_runtime_checks := false;
  let prelude = load_test_libraries () in
  let globals = TC.env0 in
  let decl = test_xform_decls Xform_wrap.xform_decls globals prelude in
  [
    ("var", `Quick, decl
      ""
      "var x : Integer;
       function f() -> Integer begin var y : Integer; x := 1; y := 1; return x + y; end"
      "var x : Integer;
       function x_Read() -> Integer begin return x; end
       function x_Write(v : Integer) begin x := v; end
       function f() -> Integer begin var y : Integer; x_Write(1); y := 1; return x_Read() + y; end");

    ("let", `Quick, decl
      ""
      "let x := 1;
       function f() -> Integer begin let y := 1; return x + y; end"
      "let x := 1;
       function f() -> Integer begin let y := 1; return x + y; end");

    ("var (array)", `Quick, decl
      ""
      "var x : array [1] of Integer;
       function f() -> Integer begin var y : array [1] of Integer; x[0] := 1; y[0] := 1; return x[0] + y[0]; end"
      "var x : array [1] of Integer;
       function x_Read(i : Integer) -> Integer begin return x[i]; end
       function x_Write(i : Integer, v : Integer) begin x[i] := v; end
       function f() -> Integer begin var y : array [1] of Integer; x_Write(0, 1); y[0] := 1; return x_Read(0) + y[0]; end");
  ]

(****************************************************************
 * Main test harness
 ****************************************************************)

let () = Alcotest.run "transforms" [
    ("wrap", wrap_tests);
  ]

(****************************************************************
 * End
 ****************************************************************)
