open Microasm.Base
open Microasm.Build
open Microasm.Interpreter
open Microasm.Trace
open Microasm.Solver.Default

let recursive_factorial =
  scope "recursive_factorial" 1
    [
      ( "check_base_case",
        [
          Const (1, value 2);
          Const (2, value 1);
          (* Command `Breakpoint; *)
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

(* x19: 0 - w20: 1 - w21: 2 - x7: 3 *)
let trace_example =
  [
    (* b .+... *)
    TBranch "B2";
    (* cbz x19, .+... NT *)
    TBranchIfZero (0, "B_", NotTaken);
    (* ldr w13, [x19, #16] - w13: 6 *)
    TConst (4, value 16);
    TBinOp (Add, 5, 0, 4);
    TLoad (6, 5, value 0x331aa148);
    (* cmp w20, w13
       b.ls .+... T *)
    TBranchIfULess (6, 1, "B_", NotTaken)
    (* w20 <=u w13 taken <=> w13 < w20 not taken *);
    (* b.eq .+... T *)
    TBinOp (Sub, 7, 6, 1);
    TBranchIfZero (7, "B3", Taken);
    (* mov x0, x19 - x0: 9 *)
    TConst (8, value 0);
    TBinOp (Add, 9, 0, 8);
    (* ldr x19, [x0] *)
    TLoad (10, 9, value 0x331aa138);
    (* b .+... *)
    TBranch "B4";
    (* cmp w21, #2
       b.ne .+... T *)
    TConst (11, value 2);
    TBinOp (Sub, 12, 2, 11);
    TBranchIfZero (12, "B_", NotTaken)
    (* w21 != 2 taken <=> w21 == 2 not taken *);
    (* b.hi .+... NT *)
    TBranchIfULess (11, 2, "B_", NotTaken) (* w21 > 2 NT *);
    (* cbnz w21, .+... T *)
    TBranchIfZero (2, "B_", NotTaken)
    (* w21 != 0 taken <=> w21 == 0 not taken *);
    (* cmp w21, #1
       b.ne .+... NT *)
    TConst (13, value 1);
    TBinOp (Sub, 14, 2, 13);
    TBranchIfZero (14, "B5", Taken) (* w21 != 1 not taken <=> w21 == 1 taken *);
    (* ldp x10, x4, [x7, #24] *)
    TConst (15, value 24);
    TBinOp (Add, 16, 3, 15);
    TLoad (17, 16, value 0x331aa150);
  ]

let main =
  scope "main" 0
    [
      ( "",
        [
          Const (0, value 5);
          Command EnableTrace;
          Call (1, "recursive_factorial", [ 0 ]);
          Command DisableTrace;
          Return 1;
        ] );
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

let solver = new solver

let assoc_list =
  solver#add_trace trace;
  match solver#generate_model () with
  | Satisfiable model -> List.of_seq (RegMap.to_seq model)
  | Unknown -> raise (Invalid_argument "Unknown")
  | NoSolution -> raise (Invalid_argument "Not satisfiable")

let result =
  solver#exclude_last ();
  solver#generate_model ()
