let identity x = x

module Register = struct
  include Int

  let hash i = i * 2654435761
end

module ScopeId = String
module BlockId = String
module IdSet = Set.Make (String)
module IdMap = Map.Make (String)
module RegSet = Set.Make (Register)

type instr =
  | Const of Register.t * Int64.t  (** Const d, c : loads c into d *)
  | Add of Register.t * Register.t * Register.t
      (** Add d, r1, r2 : addition of integers : d = r1 + r2 *)
  | Sub of Register.t * Register.t * Register.t
      (** Sub d, r1, r2 : subtraction of integers : d = r1 - r2 *)
  | Mul of Register.t * Register.t * Register.t
      (** Mul d, r1, r2 : multiplication of integers : d = (r1 * r2) & 0b111...111
        The bits that do not fit in the memory cell are ignored *)
  | Div of Register.t * Register.t * Register.t
      (** Div d, r1, r2 : signed division of integers : d = r1 // r2.
        If r2 = 0, the program halts and reports an error *)
  | Rem of Register.t * Register.t * Register.t
      (** Rem d, r1, r2 : signed integer remainder : d = r1 mod r2.
        If r2 = 0, the program halts and reports an error *)
  | Udiv of Register.t * Register.t * Register.t
      (** Div d, r1, r2 : unsigned division of integers : d = r1 // r2.
        If r2 = 0, the program halts and reports an error *)
  | Urem of Register.t * Register.t * Register.t
      (** Rem d, r1, r2 : unsigned integer remainder : d = r1 mod r2.
        If r2 = 0, the program halts and reports an error *)
  | Not of Register.t * Register.t  (** Not d, r : bitwise negation *)
  | And of Register.t * Register.t * Register.t
      (** And d, r1, r2 : bitwise and *)
  | Or of Register.t * Register.t * Register.t  (** Or d, r1, r2 : bitwise or *)
  | Xor of Register.t * Register.t * Register.t
      (** Xor d, r1, r2 : bitwise exclusive or *)
  | Branch of BlockId.t
      (** Branch target : jumps to the block `target`. Puts the current block name as the previous block encountered *)
  | BranchIfZero of Register.t * BlockId.t
      (** BranchIfZero r, target : jumps to the block `target` if r contains 0, otherwise, continues to the next instruction.
        Puts the current block name as the previous block encountered *)
  | BranchIfLess of Register.t * Register.t * BlockId.t
      (** BranchIfLess r1, r2, target : jumps to the block `target` if r1 < r2, where r1 and r2 are interpreted as signed numbers.
        Otherwise, continues to the next instruction.
        Puts the current block number as the previous block encountered *)
  | BranchIfULess of Register.t * Register.t * BlockId.t
      (** BranchIfLess r1, r2, target : jumps to the block `target` if r1 < r2, where r1 and r2 are interpreted as unsigned numbers.
        Otherwise, continues to the next instruction.
        Puts the current block number as the previous block encountered *)
  | Phi of Register.t * BlockId.t * Register.t * Register.t
      (** Phi d, b1, r1, r2 : puts r1 into d if the execution came from block b, otherwise, puts r2 into d *)
  | Call of Register.t * ScopeId.t * Register.t list
      (** Call d, name, [r1, ...] : calls the scope with id `name`, passing registers r1, ... as arguments.
        The result of the call is put in d *)
  | Return of Register.t
      (** Return r : returns the content of r to the calling scope.
        A block which does not end with a branch or a return instruction returns implicitly zero *)

type block_reference = BlockId.t * ScopeId.t

type instruction_reference = int * block_reference

type not_compliant_reason =
  | Empty_program  (** A program has no scopes but should have at least one *)
  | Bad_entry_point of ScopeId.t
  | Entry_point_has_args of ScopeId.t
  | Empty_scope of ScopeId.t
      (** A scope has no blocks but should have at least one *)
  | Bad_first_block of block_reference
  | Duplicate_register of Register.t * instruction_reference
  | Block_not_found of BlockId.t * instruction_reference
  | Scope_not_found of ScopeId.t * instruction_reference
  | Invalid_argument_count of int * int * ScopeId.t * instruction_reference
      (** Arguments given, arguments expected, scope name, ... *)

exception Not_compliant of not_compliant_reason list

type block_builder = BlockId.t * instr list

type scope_builder = ScopeId.t * int * BlockId.t option * block_builder list

type program_builder = ScopeId.t * scope_builder list

let value = Int64.of_int

let scope (name : ScopeId.t) (args_count : int) ?entry
    (builder_list : block_builder list) =
  (name, args_count, entry, builder_list)
