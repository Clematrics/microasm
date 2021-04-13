open Microasm.Base
open Microasm.Build
open Microasm.Interpreter

let run prog =
  let exe = new execution prog in
  exe#execute Continue;
  exe#result ()

let test_add =
  let prog a b =
    let scope a b =
      scope "test" 0
        [
          ( "setup",
            [ Const (0, a); Const (1, b); BinOp (Add, 2, 0, 1); Return 2 ] );
        ]
    in
    program ("test", [ scope a b ])
  in
  assert (run (prog (value 0) (value 0)) = value 0);
  assert (
    run (prog (Int64.of_string "0x7FFF_FFFF_FFFF_FFFF") (value 1))
    = Int64.min_int)

let test_mul =
  let prog a b =
    let scope a b =
      scope "test" 0
        [
          ( "setup",
            [ Const (0, a); Const (1, b); BinOp (Mul, 2, 0, 1); Return 2 ] );
        ]
    in
    program ("test", [ scope a b ])
  in
  assert (run (prog (value 0) (value 0)) = value 0);
  assert (
    run
      (prog (Int64.of_string "0x1_0000_0000") (Int64.of_string "0x1_0000_0000"))
    = value 0);
  assert (
    run (prog (Int64.of_string "0x8000_0000_0000_0001") (value 2)) = value 2);
  assert (
    run (prog (Int64.of_string "0x8000_0000_0000_0001") (value (-2)))
    = value (-2));
  assert (run (prog (value (-1)) (value 10)) = value (-10))

let test_div = ()

let test_rem = ()

let test_udiv = ()

let test_urem = ()

let test_branch_less =
  let prog a b =
    let scope a b =
      scope "test" 0
        [
          ( "setup",
            [
              Const (0, a);
              Const (1, b);
              BranchIfLess (0, 1, "taken");
              Const (2, value 0);
              Return 2;
            ] );
          ("taken", [ Const (3, value 1); Return 3 ]);
        ]
    in
    program ("test", [ scope a b ])
  in
  assert (run (prog (value 0) (value 0)) = value 0);
  assert (run (prog (value 0) (value 100)) = value 1);
  assert (run (prog (value 100) (value 0)) = value 0);
  assert (run (prog (value (-10)) (value 10)) = value 1);
  assert (run (prog (value 10) (value (-10))) = value 0);
  assert (run (prog (value 100) Int64.min_int) = value 0);
  assert (run (prog Int64.min_int (value 100)) = value 1);
  assert (run (prog Int64.max_int Int64.min_int) = value 0);
  assert (run (prog Int64.min_int Int64.max_int) = value 1)

let test_branch_uless =
  let prog a b =
    let scope a b =
      scope "test" 0
        [
          ( "setup",
            [
              Const (0, a);
              Const (1, b);
              BranchIfULess (0, 1, "taken");
              Const (2, value 0);
              Return 2;
            ] );
          ("taken", [ Const (3, value 1); Return 3 ]);
        ]
    in
    program ("test", [ scope a b ])
  in
  assert (run (prog (value 0) (value 0)) = value 0);
  assert (run (prog (value 0) (value 100)) = value 1);
  assert (run (prog (value 100) (value 0)) = value 0);
  assert (run (prog (value (-10)) (value 10)) = value 0);
  assert (run (prog (value 100) Int64.min_int) = value 1);
  assert (run (prog Int64.max_int Int64.min_int) = value 1)

let test_phi =
  let prog1 =
    let scope =
      scope "test" 0
        [
          ("a", [ Const (0, value 0); Branch "b" ]);
          ("b", [ Const (1, value 1); Branch "result" ]);
          ("result", [ Phi (2, "a", 0, 1); Return 2 ]);
        ]
    in
    program ("test", [ scope ])
  in
  let prog2 =
    let scope =
      scope "test" 0
        [
          ("a", [ Const (0, value 0); Branch "b" ]);
          ("b", [ Const (1, value 1); Branch "result" ]);
          ("result", [ Phi (2, "b", 1, 0); Return 2 ]);
        ]
    in
    program ("test", [ scope ])
  in
  let prog3 =
    let scope =
      scope "test" 0
        [
          ("a", [ Const (0, value 0); Branch "result" ]);
          ("b", [ Const (1, value 1); Branch "result" ]);
          ("result", [ Phi (2, "a", 0, 1); Return 2 ]);
        ]
    in
    program ("test", [ scope ])
  in
  let prog4 =
    let scope =
      scope "test" 0
        [
          ("a", [ Const (0, value 0); Branch "result" ]);
          ("b", [ Const (1, value 1); Branch "result" ]);
          ("result", [ Phi (2, "b", 1, 0); Return 2 ]);
        ]
    in
    program ("test", [ scope ])
  in
  let prog5 =
    let scope =
      scope "test" 0
        [
          ("a", [ Const (0, value 0); Branch "result" ]);
          ("result", [ Phi (2, "a", 1, 0); Return 2 ]);
        ]
    in
    program ("test", [ scope ])
  in
  assert (run prog1 = value 1);
  assert (run prog2 = value 1);
  assert (run prog3 = value 0);
  assert (run prog4 = value 0);
  try
    let _ = run prog5 in
    assert false
  with Undefined_register _ -> ()
