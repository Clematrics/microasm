open Microasm.Trace
open Microasm.Solver.Default

let value = Int64.of_int

(* x19: 0 - w20: 1 - w21: 2 - x7: 3 *)
let trace =
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

let solver = new solver

let assoc_list =
  solver#add_trace trace;
  match solver#generate_model () with
  | Satisfiable model -> List.of_seq (RegMap.to_seq model)
  | Unknown -> raise (Invalid_argument "Unknown")
  | NoSolution -> raise (Invalid_argument "Not satisfiable")

let _ = assoc_list
