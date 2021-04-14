module Register = struct
  include Int

  let hash = ( * ) 2654435761
end

module type OrderedString = sig
  type t = string

  val compare : t -> t -> int
end

module BlockId = String
module ScopeId = String
module IdSet = Set.Make (String)

type instr_command = EnableTrace | DisableTrace | Breakpoint

type bin_op = Add | Sub | Mul | Div | Rem | Udiv | Urem | And | Or | Xor

type instr =
  | Const of Register.t * Int64.t
  | BinOp of bin_op * Register.t * Register.t * Register.t
  | Not of Register.t * Register.t
  | Branch of BlockId.t
  | BranchIfZero of Register.t * BlockId.t
  | BranchIfLess of Register.t * Register.t * BlockId.t
  | BranchIfULess of Register.t * Register.t * BlockId.t
  | Phi of Register.t * BlockId.t * Register.t * Register.t
  | Call of Register.t * ScopeId.t * Register.t list
  | Return of Register.t
  | Command of instr_command

type block_reference = BlockId.t * ScopeId.t

type instruction_reference = int * block_reference

let value = Int64.of_int

type block_builder = BlockId.t * instr list

type scope_builder = ScopeId.t * int * BlockId.t option * block_builder list

type program_builder = ScopeId.t * scope_builder list
