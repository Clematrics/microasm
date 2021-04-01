open Base
module RegisterFile = Map.Make (Register)

type register_file = Register.t RegisterFile.t

type cond_annotation = Taken | NotTaken

type phi_annotation = FirstSelected | OtherSelected

type trace_instr =
  | TConst of Register.t * Int64.t  (** Const d, c : loads c into d *)
  | TBinOp of bin_op * Register.t * Register.t * Register.t
  | TNot of Register.t * Register.t  (** Not d, r : bitwise negation *)
  | TBranch of BlockId.t
      (** Branch target : jumps to the block `target`. Puts the current block name as the previous block encountered *)
  | TBranchIfZero of Register.t * BlockId.t * cond_annotation
      (** BranchIfZero r, target : jumps to the block `target` if r contains 0, otherwise, continues to the next instruction.
      Puts the current block name as the previous block encountered *)
  | TBranchIfLess of Register.t * Register.t * BlockId.t * cond_annotation
      (** BranchIfLess r1, r2, target : jumps to the block `target` if r1 < r2, where r1 and r2 are interpreted as signed numbers.
      Otherwise, continues to the next instruction.
      Puts the current block number as the previous block encountered *)
  | TBranchIfULess of Register.t * Register.t * BlockId.t * cond_annotation
      (** BranchIfLess r1, r2, target : jumps to the block `target` if r1 < r2, where r1 and r2 are interpreted as unsigned numbers.
      Otherwise, continues to the next instruction.
      Puts the current block number as the previous block encountered *)
  | TPhi of
      Register.t
      * BlockId.t
      * Register.t option
      * Register.t option
      * phi_annotation
      (** Phi d, b1, r1, r2 : puts r1 into d if the execution came from block b, otherwise, puts r2 into d *)
  | TCall of Register.t list * ScopeId.t * Register.t list
      (** TCall [d1, ...] name, [r1, ...] : calls the scope with id `name`, passing registers r1, ... as arguments.
      The result of the call is put in d *)
  | TReturn of Register.t * Register.t
      (** Return r : returns the content of r to the calling scope.
      A block which does not end with a branch or a return instruction returns implicitly zero *)
  | TStop of Register.t

let linearize trace =
  let next_lin_reg = ref 0 in
  let linearize_instruction reg_file instr =
    let linearize_input input = RegisterFile.find input reg_file in
    let linearize_output reg_file output =
      let new_lin_reg = !next_lin_reg in
      incr next_lin_reg;
      (RegisterFile.add output new_lin_reg reg_file, new_lin_reg)
    in
    let new_reg_file, new_instr =
      match instr with
      | TConst (d, v) ->
          let file, ld = linearize_output reg_file d in
          (file, TConst (ld, v))
      | TBinOp (op, d, r1, r2) ->
          let lr1 = linearize_input r1
          and lr2 = linearize_input r2
          and file, ld = linearize_output reg_file d in
          (file, TBinOp (op, ld, lr1, lr2))
      | TNot (d, r) ->
          let lr = linearize_input r
          and file, ld = linearize_output reg_file d in
          (file, TNot (ld, lr))
      | TBranch t -> (reg_file, TBranch t)
      | TBranchIfZero (r, t, a) ->
          let lr = linearize_input r in
          (reg_file, TBranchIfZero (lr, t, a))
      | TBranchIfLess (r1, r2, t, a) ->
          let lr1 = linearize_input r1 and lr2 = linearize_input r2 in
          (reg_file, TBranchIfLess (lr1, lr2, t, a))
      | TBranchIfULess (r1, r2, t, a) ->
          let lr1 = linearize_input r1 and lr2 = linearize_input r2 in
          (reg_file, TBranchIfULess (lr1, lr2, t, a))
      | TPhi (d, s, r1, r2, a) ->
          let lr1 = Option.map linearize_input r1
          and lr2 = Option.map linearize_input r2
          and file, ld = linearize_output reg_file d in
          (file, TPhi (ld, s, lr1, lr2, a))
      | TCall (ds, t, rs) ->
          let lrs = List.map linearize_input rs
          and file, lds = List.fold_left_map linearize_output reg_file ds in
          (file, TCall (lds, t, lrs))
      | TReturn (d, r) ->
          let lr = linearize_input r
          and file, ld = linearize_output reg_file d in
          (file, TReturn (ld, lr))
      | TStop r ->
          let lr = linearize_input r in
          (reg_file, TStop lr)
    in
    (new_reg_file, new_instr)
  in
  let _, linearized =
    List.fold_left_map
      (fun reg_file instr ->
        let new_reg_file, new_instr = linearize_instruction reg_file instr in
        (new_reg_file, new_instr))
      RegisterFile.empty trace
  in
  linearized
