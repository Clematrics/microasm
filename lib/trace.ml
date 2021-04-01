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
| TPhi of Register.t * BlockId.t * Register.t option * Register.t option * phi_annotation
    (** Phi d, b1, r1, r2 : puts r1 into d if the execution came from block b, otherwise, puts r2 into d *)
| TCall of Register.t list * ScopeId.t * Register.t list
    (** TCall [d1, ...] name, [r1, ...] : calls the scope with id `name`, passing registers r1, ... as arguments.
      The result of the call is put in d *)
| TReturn of Register.t * Register.t
    (** Return r : returns the content of r to the calling scope.
      A block which does not end with a branch or a return instruction returns implicitly zero *)
| TStop of Register.t
