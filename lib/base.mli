(** {1 Modules} *)

(** Registers are ints with extended capabilities to identifies memory cells in
    a program *)
module Register : sig
  include module type of Int

  val hash : t -> t
  (** Basic hash function for hash tables *)
end

module type OrderedString = sig
  type t = string

  val compare : t -> t -> int
end

module ScopeId : OrderedString
(** Type and functions to identify scopes *)

module BlockId : OrderedString
(** Type and functions to identify blocks *)

module IdSet : module type of Set.Make (String)
(** Type and functions to identify blocks *)

(** {1 Instructions} *)

type instr_command =
  | EnableTrace
  | DisableTrace
  | Breakpoint
      (** Commands that can be embedded in a program to affect the interpreter
          when encountered

          - [`EnableTrace] enables tracing starting at the next instruction
          - [`DisableTrace] disables tracing
          - [`Breakpoint] breaks the execution and starts the debugger at the
            next instruction *)

(** Binary operations *)
type bin_op =
  | Add  (** [(Add, d, r1, r2)]: addition of integers: [d = r1 + r2] *)
  | Sub  (** [(Sub, d, r1, r2)]: subtraction of integers: [d = r1 - r2] *)
  | Mul
      (** [(Mul, d, r1, r2)]: multiplication of integers: [d = r1 * r2]. Bits
          that do not fit in the memory cell are discarded *)
  | Div
      (** [(Div, d, r1, r2)]: signed division of integers: [d = r1 // r2]. If
          [r2 = 0], the program halts and reports an error *)
  | Rem
      (** [(Rem, d, r1, r2)]: signed integer remainder: [d = r1 mod r2]. If
          [r2 = 0], the program halts and reports an error *)
  | Udiv
      (** [(Div, d, r1, r2)]: unsigned division of integers: [d = r1 // r2]. If
          [r2 = 0], the program halts and reports an error *)
  | Urem
      (** [(Rem, d, r1, r2)]: unsigned integer remainder: [d = r1 mod r2]. If
          [r2 = 0], the program halts and reports an error *)
  | And  (** [(And, d, r1, r2)]: bitwise and *)
  | Or  (** [(Or, d, r1, r2)]: bitwise or *)
  | Xor  (** [(Xor, d, r1, r2)]: bitwise exclusive or *)

type instr =
  | Const of Register.t * Int64.t
      (** [Const (d, c)]: loads [c] into register [d] *)
  | BinOp of bin_op * Register.t * Register.t * Register.t
  | Not of Register.t * Register.t  (** [Not d, r]: bitwise negation *)
  | Branch of BlockId.t
      (** [Branch target]: jumps to the block [target]. Puts the current block
          name as the previous block encountered *)
  | BranchIfZero of Register.t * BlockId.t
      (** [BranchIfZero r, target]: jumps to the block [target] if [r] contains
          [0], otherwise, continues to the next instruction. Puts the current
          block name as the previous block encountered *)
  | BranchIfLess of Register.t * Register.t * BlockId.t
      (** [BranchIfLess r1, r2, target]: jumps to the block [target] if
          [r1 < r2], where [r1] and [r2] are interpreted as signed numbers.
          Otherwise, continues to the next instruction. Puts the current block
          number as the previous block encountered *)
  | BranchIfULess of Register.t * Register.t * BlockId.t
      (** [BranchIfLess r1, r2, target]: jumps to the block [target] if
          [r1 < r2], where [r1] and [r2] are interpreted as unsigned numbers.
          Otherwise, continues to the next instruction. Puts the current block
          number as the previous block encountered *)
  | Phi of Register.t * BlockId.t * Register.t * Register.t
      (** [Phi d, b1, r1, r2]: puts [r1] into [d] if the execution came from
          block [b], otherwise, puts [r2] into [d] *)
  | Call of Register.t * ScopeId.t * Register.t list
      (** [Call d, name, args]: calls the scope with id [name], passing
          registers in the list [args] as arguments. The result of the call is
          put in [d] *)
  | Return of Register.t
      (** [Return r]: returns the content of [r] to the calling scope. *)
  | Command of instr_command

type block_reference = BlockId.t * ScopeId.t
(** Reference to a block in a program *)

type instruction_reference = int * block_reference
(** Reference to an instruction (by its index in its block) in a program *)

val value : int -> int64
(** Helper function to introduce constants in programs *)

type block_builder = BlockId.t * instr list
(** Type used to build a block [(name, instr_list)]: [name] of the block,
    [instr_list] is the list of instructions inside *)

type scope_builder = ScopeId.t * int * BlockId.t option * block_builder list
(** Type used to build a scope [(name, args_count, entry_opt, blocks)]: [name]
    of the scope, [args_count] is the number of parameters, [entry_opt] is the
    name of the entry block *)

type program_builder = ScopeId.t * scope_builder list
(** Type used to build a program [(entry, scopes)]: [entry] of the program *)
