open Microasm.Base
open Microasm.Build
open Microasm.Interpreter
open Microasm.Solver

let recursive_factorial =
  scope "recursive_factorial" 1
    [
      ( "check_base_case",
        [
          Const (1, value 2);
          Const (2, value 1);
          BranchIfLess (0, 1, "return");
          Branch "main_case";
        ] );
      ( "main_case",
        [
          BinOp (Sub, 3, 0, 2);
          Call (4, "recursive_factorial", [ 3 ]);
          BinOp (Mul, 5, 4, 0);
          Branch "return";
        ] );
      ("return", [ Phi (6, "main_case", 5, 2); Return 6 ]);
    ]

let loop_factorial =
  scope "loop_factorial" 1
    [
      ("init_loop", [ Const (1, value 1); Branch "loop_body" ]);
      ( "loop_body",
        [
          Phi (2, "init_loop", 1, 4);
          Phi (3, "init_loop", 0, 5);
          BinOp (Mul, 4, 2, 3);
          BinOp (Sub, 5, 3, 1);
          BranchIfLess (5, 1, "return");
          Branch "loop_body";
        ] );
      ("return", [ Return 2 ]);
    ]

let main =
  scope "main" 0
    [
      ("", [ Const (0, value 5); Call (1, "loop_factorial", [ 0 ]); Return 1 ]);
    ]

let factorial_test =
  program ("main", [ main; recursive_factorial; loop_factorial ])

(* let () = *)
let engine = new execution ~enable_trace:true factorial_test

let () =
  engine#execute Continue;
  Printf.printf "%Lu\n" (engine#result ())

let full_trace = engine#trace ()

let trace = List.tl full_trace

let model, map = smt_converter trace

let get r =
  let var = RegMap.find r map in
  match Z3.Model.get_const_interp_e model var with
  | None -> raise (Invalid_argument "Variable not found in model")
  | Some expr -> Z3.Expr.to_string expr

let values = List.init (RegMap.cardinal map) get
