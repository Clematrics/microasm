open Base
open Trace
open Z3
open Z3.BitVector
open Z3.Boolean
open Z3.Solver
open Z3.Expr
module RegMap = Map.Make (Register)

let reg r = Printf.sprintf "r%d" r

let make_const ctx r reg_map =
  if not (RegMap.mem r reg_map) then
    let var = BitVector.mk_const_s ctx (reg r) 64 in
    RegMap.add r var reg_map
  else reg_map

(** Create a variable for each register seen in an instruction, event for the source registers *)
let create_var ctx reg_map tinstr =
  match tinstr with
  | TConst (d, _) -> make_const ctx d reg_map
  | TBinOp (_, d, r1, r2) ->
      make_const ctx r1 reg_map |> make_const ctx r2 |> make_const ctx d
  | TNot (d, r) -> make_const ctx r reg_map |> make_const ctx d
  | TBranch _ -> reg_map
  | TBranchIfZero (r, _, _) -> make_const ctx r reg_map
  | TBranchIfLess (r1, r2, _, _) ->
      make_const ctx r1 reg_map |> make_const ctx r2
  | TBranchIfULess (r1, r2, _, _) ->
      make_const ctx r1 reg_map |> make_const ctx r2
  | TPhi (d, _, r1, r2, _) ->
      let map' =
        Option.fold ~none:reg_map ~some:(fun r -> make_const ctx r reg_map) r1
      in
      let map'' =
        Option.fold ~none:map' ~some:(fun r -> make_const ctx r map') r2
      in
      make_const ctx d map''
  | TCall (ds, _, rs) ->
      let new_map =
        List.fold_left (fun map r -> make_const ctx r map) reg_map rs
      in
      List.fold_left (fun map r -> make_const ctx r map) new_map ds
  | TReturn (d, r) -> make_const ctx r reg_map |> make_const ctx d
  | TStop r -> make_const ctx r reg_map

let smt_of_instr ctx solver reg_map tinstr =
  let sort = BitVector.mk_sort ctx 64 in
  let get_reg r =
    try RegMap.find r reg_map with _ -> assert false
    (* Should not happen *)
  in
  match tinstr with
  | TConst (d, v) ->
      let cst = mk_numeral_string ctx (Int64.to_string v) sort in
      let dest = get_reg d in
      Solver.add solver [ mk_eq ctx dest cst ]
  | TBinOp (op, d, r1, r2) ->
      let r1 = get_reg r1 and r2 = get_reg r2 and dest = get_reg d in
      let op_expr =
        match op with
        | Add -> BitVector.mk_add ctx r1 r2
        | Sub -> BitVector.mk_sub ctx r1 r2
        | Mul -> BitVector.mk_mul ctx r1 r2
        | Div -> BitVector.mk_sdiv ctx r1 r2
        | Rem -> BitVector.mk_srem ctx r1 r2
        | Udiv -> BitVector.mk_udiv ctx r1 r2
        | Urem -> BitVector.mk_urem ctx r1 r2
        | And -> BitVector.mk_and ctx r1 r2
        | Or -> BitVector.mk_or ctx r1 r2
        | Xor -> BitVector.mk_xor ctx r1 r2
      in
      Solver.add solver [ mk_eq ctx dest op_expr ]
  | TNot (d, r) ->
      let src = get_reg r and dest = get_reg d in
      Solver.add solver [ mk_eq ctx dest (BitVector.mk_not ctx src) ]
  | TBranch _ -> ()
  | TBranchIfZero (r, _, a) ->
      let src = get_reg r in
      let zero = mk_numeral_int ctx 0 sort in
      let expr =
        match a with
        | Taken -> mk_eq ctx src zero
        | NotTaken -> mk_distinct ctx [ src; zero ]
      in
      Solver.add solver [ expr ]
  | TBranchIfLess (r1, r2, _, a) ->
      let r1 = get_reg r1 and r2 = get_reg r2 in
      let expr =
        match a with Taken -> mk_slt ctx r1 r2 | NotTaken -> mk_sge ctx r1 r2
      in
      Solver.add solver [ expr ]
  | TBranchIfULess (r1, r2, _, a) ->
      let r1 = get_reg r1 and r2 = get_reg r2 in
      let expr =
        match a with Taken -> mk_ult ctx r1 r2 | NotTaken -> mk_uge ctx r1 r2
      in
      Solver.add solver [ expr ]
  | TPhi (d, _, r1, r2, a) ->
      let dest = get_reg d in
      let r = match a with FirstSelected -> r1 | OtherSelected -> r2 in
      let src = get_reg (Option.get r) in
      let expr = mk_eq ctx dest src in
      Solver.add solver [ expr ]
  | TCall (ds, _, rs) ->
      let exprs =
        List.rev_map2
          (fun d r ->
            let src = get_reg r and dest = get_reg d in
            mk_eq ctx dest src)
          ds rs
      in
      Solver.add solver exprs
  | TReturn (d, r) ->
      let src = get_reg r and dest = get_reg d in
      Solver.add solver [ mk_eq ctx dest src ]
  | TStop _ -> ()

let smt_converter trace =
  let cfg = [ ("model", "true"); ("proof", "false") ] in
  let ctx = mk_context cfg in
  let solver = mk_simple_solver ctx in
  let reg_map = List.fold_left (create_var ctx) RegMap.empty trace in
  List.iter (smt_of_instr ctx solver reg_map) trace;
  match Solver.check solver [] with
  | UNSATISFIABLE -> raise (Error "No solution exists")
  | UNKNOWN ->
      raise (Error "Couldn't prove the existence or the absence of solutions")
  | SATISFIABLE -> (
      match Solver.get_model solver with
      | None -> assert false
      | Some model -> (model, reg_map))
