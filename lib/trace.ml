open Base
module RegisterFile = Map.Make (Register)

type register_file = Register.t RegisterFile.t

type cond_annotation = Taken | NotTaken

type phi_annotation = FirstSelected | OtherSelected

type trace_instr =
  | TConst of Register.t * Int64.t  (** Const d, c : loads c into d *)
  | TBinOp of bin_op * Register.t * Register.t * Register.t
  | TNot of Register.t * Register.t  (** Not d, r : bitwise negation *)
  | TLoad of Register.t * Register.t * Int64.t (** Load d, addr, val : load zero to d, and addr = val *)
  | TStore of Register.t * Register.t * Int64.t  (** Store r, addr, val : store r to addr, with addr = val *)
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

module ConstList = struct
  type zero = unit

  type 'size succ = 'size * unit

  type ('elt, 'size) t =
    | [] : ('elt, zero) t
    | ( :: ) : 'elt * ('elt, 'size) t -> ('elt, 'size succ) t

  let fold_left_map f acc clist =
    let acc = ref acc in
    let rec internal : type size. (_, size) t -> (_, size) t = function
      | [] -> []
      | hd :: tl ->
          let acc', b = f !acc hd in
          acc := acc';
          b :: internal tl
    in
    (!acc, internal clist)
end

let linearize trace =
  let next_lin_reg = ref 0 in
  let linearize_instruction reg_file instr =
    let linearize_reg reg_file reg =
      match RegisterFile.find_opt reg reg_file with
      | Some p -> (reg_file, p)
      | None ->
          let new_lin_reg = !next_lin_reg in
          incr next_lin_reg;
          (RegisterFile.add reg new_lin_reg reg_file, new_lin_reg)
    in
    let linearize_const_list clist =
      (* Only used when everything can be applied to the base reg_file.
         Do not use when you want to use a different register file than the one given in parameter *)
      ConstList.fold_left_map linearize_reg reg_file clist
    in
    let new_reg_file, new_instr =
      match instr with
      | TConst (d, v) ->
          let file, [ ld ] = linearize_const_list [ d ] in
          (file, TConst (ld, v))
      | TBinOp (op, d, r1, r2) ->
          let file, [ lr1; lr2; ld ] = linearize_const_list [ r1; r2; d ] in
          (file, TBinOp (op, ld, lr1, lr2))
      | TNot (d, r) ->
          let file, [ lr; ld ] = linearize_const_list [ r; d ] in
          (file, TNot (ld, lr))
      | TLoad (d, addr, value) ->
          let file, [ laddr; ld ] = linearize_const_list [ addr; d ] in
          (file, TLoad (ld, laddr, value))
      | TStore (r, addr, value) ->
          let file, [ laddr; lr ] = linearize_const_list [ addr; r ] in
          (file, TStore (lr, laddr, value))
      | TBranch t -> (reg_file, TBranch t)
      | TBranchIfZero (r, t, a) ->
          let file, [ lr ] = linearize_const_list [ r ] in
          (file, TBranchIfZero (lr, t, a))
      | TBranchIfLess (r1, r2, t, a) ->
          let file, [ lr1; lr2 ] = linearize_const_list [ r1; r2 ] in
          (file, TBranchIfLess (lr1, lr2, t, a))
      | TBranchIfULess (r1, r2, t, a) ->
          let file, [ lr1; lr2 ] = linearize_const_list [ r1; r2 ] in
          (file, TBranchIfULess (lr1, lr2, t, a))
      | TPhi (d, s, r1, r2, a) ->
          let opt file =
            Option.fold ~none:(file, None) ~some:(fun r ->
                let file', r = linearize_reg file r in
                (file', Some r))
          in
          let file, [ lr1; lr2 ] =
            ConstList.fold_left_map opt reg_file [ r1; r2 ]
          in
          let file', ld = linearize_reg file d in
          (file', TPhi (ld, s, lr1, lr2, a))
      | TCall (ds, t, rs) ->
          let file, [ lrs; lds ] =
            ConstList.fold_left_map
              (List.fold_left_map linearize_reg)
              reg_file [ rs; ds ]
          in
          (file, TCall (lds, t, lrs))
      | TReturn (d, r) ->
          let file, [ lr; ld ] = linearize_const_list [ r; d ] in
          (file, TReturn (ld, lr))
      | TStop r ->
          let file, [ lr ] = linearize_const_list [ r ] in
          (file, TStop lr)
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
