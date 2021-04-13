open Microasm.Trace
open Microasm.Solver

module S = Solver (struct
  let bit_width = 10
  (* Not too large because testing div, rem, udiv & urem is exponential
     (it seems that all possibilities are tested) *)
end)

open S
open Z3

let setup () =
  let cfg = [ ("model", "true"); ("proof", "false") ] in
  let ctx = mk_context cfg in
  let solver = Solver.mk_simple_solver ctx in
  (ctx, solver)

let trace_to_constraints ctx trace =
  let reg_map = List.fold_left (create_var ctx) RegMap.empty trace in
  (List.concat (List.map (smt_of_instr ctx reg_map) trace), reg_map)

let test_div_rem =
  let trace = [ TBinOp (Div, 2, 0, 1); TBinOp (Rem, 3, 0, 1) ] in
  let ctx, solver = setup () in
  let exprs, reg_map = trace_to_constraints ctx trace in
  let rel =
    Boolean.mk_distinct ctx
      [
        RegMap.find 0 reg_map;
        BitVector.mk_add ctx
          (BitVector.mk_mul ctx (RegMap.find 2 reg_map) (RegMap.find 1 reg_map))
          (RegMap.find 3 reg_map);
      ]
  in
  Solver.add solver (rel :: exprs);
  match Solver.check solver [] with
  | UNSATISFIABLE -> ()
  | UNKNOWN -> Printf.printf "Unknown\n"
  | SATISFIABLE -> (
      match Solver.get_model solver with
      | Some model ->
          Printf.printf "%s\n" (Model.to_string model);
          assert false
      | None -> assert false (* Not reachable *))

let test_udiv_urem =
  let trace = [ TBinOp (Udiv, 2, 0, 1); TBinOp (Urem, 3, 0, 1) ] in
  let ctx, solver = setup () in
  let exprs, reg_map = trace_to_constraints ctx trace in
  let rel =
    Boolean.mk_distinct ctx
      [
        RegMap.find 0 reg_map;
        BitVector.mk_add ctx
          (BitVector.mk_mul ctx (RegMap.find 2 reg_map) (RegMap.find 1 reg_map))
          (RegMap.find 3 reg_map);
      ]
  in
  Solver.add solver (rel :: exprs);
  match Solver.check solver [] with
  | UNSATISFIABLE -> ()
  | UNKNOWN -> Printf.printf "Unknown\n"
  | SATISFIABLE -> (
      match Solver.get_model solver with
      | Some model ->
          Printf.printf "%s\n" (Model.to_string model);
          assert false
      | None -> assert false (* Not reachable *))
