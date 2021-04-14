open Base
open Trace
open Z3
module RegMap = Map.Make (Register)

module Solver (Params : sig
  val bit_width : int
end) =
struct
  let bit_width = Params.bit_width

  let reg r = Printf.sprintf "r%d" r

  let make_const ctx r reg_map =
    if not (RegMap.mem r reg_map) then
      let var = BitVector.mk_const_s ctx (reg r) bit_width in
      RegMap.add r var reg_map
    else reg_map

  let create_var ctx reg_map tinstr =
    match tinstr with
    | TConst (d, _) -> make_const ctx d reg_map
    | TBinOp (_, d, r1, r2) ->
        make_const ctx r1 reg_map |> make_const ctx r2 |> make_const ctx d
    | TNot (d, r) -> make_const ctx r reg_map |> make_const ctx d
    | TLoad (d, addr, _) -> make_const ctx addr reg_map |> make_const ctx d
    | TStore (r, addr, _) -> make_const ctx addr reg_map |> make_const ctx r
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

  let smt_of_instr ctx reg_map tinstr =
    let sort = BitVector.mk_sort ctx bit_width in
    let get_reg r =
      try RegMap.find r reg_map with _ -> assert false
      (* Should not happen *)
    in
    match tinstr with
    | TConst (d, v) ->
        let cst = Expr.mk_numeral_string ctx (Int64.to_string v) sort in
        let dest = get_reg d in
        [ Boolean.mk_eq ctx dest cst ]
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
        [ Boolean.mk_eq ctx dest op_expr ]
    | TNot (d, r) ->
        let src = get_reg r and dest = get_reg d in
        [ Boolean.mk_eq ctx dest (BitVector.mk_not ctx src) ]
    | TLoad (_, addr, value) ->
        let addr = get_reg addr
        and cst = Expr.mk_numeral_string ctx (Int64.to_string value) sort in
        [ Boolean.mk_eq ctx addr cst ]
        (* There is no information about d *)
    | TStore (_, addr, value) ->
        let addr = get_reg addr
        and cst = Expr.mk_numeral_string ctx (Int64.to_string value) sort in
        [ Boolean.mk_eq ctx addr cst ]
        (* There is no information about r *)
    | TBranch _ -> []
    | TBranchIfZero (r, _, a) ->
        let src = get_reg r in
        let zero = Expr.mk_numeral_int ctx 0 sort in
        let expr =
          match a with
          | Taken -> Boolean.mk_eq ctx src zero
          | NotTaken -> Boolean.mk_distinct ctx [ src; zero ]
        in
        [ expr ]
    | TBranchIfLess (r1, r2, _, a) ->
        let r1 = get_reg r1 and r2 = get_reg r2 in
        let expr =
          match a with
          | Taken -> BitVector.mk_slt ctx r1 r2
          | NotTaken -> BitVector.mk_sge ctx r1 r2
        in
        [ expr ]
    | TBranchIfULess (r1, r2, _, a) ->
        let r1 = get_reg r1 and r2 = get_reg r2 in
        let expr =
          match a with
          | Taken -> BitVector.mk_ult ctx r1 r2
          | NotTaken -> BitVector.mk_uge ctx r1 r2
        in
        [ expr ]
    | TPhi (d, _, r1, r2, a) ->
        let dest = get_reg d in
        let r = match a with FirstSelected -> r1 | OtherSelected -> r2 in
        let src = get_reg (Option.get r) in
        let expr = Boolean.mk_eq ctx dest src in
        [ expr ]
    | TCall (ds, _, rs) ->
        let exprs =
          List.rev_map2
            (fun d r ->
              let src = get_reg r and dest = get_reg d in
              Boolean.mk_eq ctx dest src)
            ds rs
        in
        exprs
    | TReturn (d, r) ->
        let src = get_reg r and dest = get_reg d in
        [ Boolean.mk_eq ctx dest src ]
    | TStop _ -> []

  let smt_converter trace =
    let cfg = [ ("model", "true"); ("proof", "false") ] in
    let ctx = mk_context cfg in
    let solver = Solver.mk_simple_solver ctx in
    let reg_map = List.fold_left (create_var ctx) RegMap.empty trace in
    List.iter
      (fun tinstr -> Solver.add solver (smt_of_instr ctx reg_map tinstr))
      trace;
    match Solver.check solver [] with
    | UNSATISFIABLE -> raise (Error "No solution exists")
    | UNKNOWN ->
        raise (Error "Couldn't prove the existence or the absence of solutions")
    | SATISFIABLE -> (
        match Solver.get_model solver with
        | None -> assert false
        | Some model -> (model, reg_map))

  type solver_result = NoSolution | Unknown | Satisfiable of Int64.t RegMap.t
  (* TODO: make specific type? *)

  class solver =
    let cfg = [ ("model", "true"); ("proof", "false") ] in
    let ctx = mk_context cfg in
    let solver = Solver.mk_simple_solver ctx in
    let sort_64 = BitVector.mk_sort ctx bit_width in
    object (self)
      val solver = solver

      val mutable reg_map = RegMap.empty

      val mutable model = RegMap.empty

      method solver = solver

      method add_trace trace =
        (* TODO: add hash function *)
        reg_map <- List.fold_left (create_var ctx) reg_map trace;
        List.iter
          (fun tinstr -> Solver.add solver (smt_of_instr ctx reg_map tinstr))
          trace

      method private decompose_model raw_model =
        let value_of_string str =
          let correct_str =
            String.concat "" [ "0"; String.sub str 1 (String.length str - 1) ]
          in
          Int64.of_string correct_str
        in
        model <-
          RegMap.map
            (fun var ->
              match Z3.Model.get_const_interp_e raw_model var with
              | None ->
                  Printf.printf "Manque %s" (Z3.Expr.to_string var);
                  Int64.zero (* assert false *)
              | Some expr -> value_of_string @@ Expr.to_string expr)
            reg_map;
        model

      method generate_model () =
        match Solver.check solver [] with
        | UNSATISFIABLE -> NoSolution
        | UNKNOWN -> Unknown
        | SATISFIABLE -> (
            match Solver.get_model solver with
            | None -> assert false
            | Some model -> Satisfiable (self#decompose_model model))

      method exclude_last () =
        let equalities =
          Stdlib.Seq.map
            (fun (reg, value) ->
              let const =
                Expr.mk_numeral_string ctx (Int64.to_string value) sort_64
              in
              let reg_const = RegMap.find reg reg_map in
              Boolean.mk_distinct ctx [ const; reg_const ])
            (RegMap.to_seq model)
        in
        let expr = Boolean.mk_or ctx (List.of_seq equalities) in
        Solver.add solver [ expr ]
    end
end

module Default = Solver (struct
  let bit_width = 64
end)
