open Base

module Block : sig
  type t =
    < name : BlockId.t
    ; instructions : instr array
    ; instruction : int -> instr >
end

module Scope : sig
  type t =
    < name : ScopeId.t
    ; args_count : int
    ; block : BlockId.t -> Block.t
    ; entry_block : Block.t >
end

module Program : sig
  type t =
    < entry : ScopeId.t ; scope : ScopeId.t -> Scope.t ; entry_scope : Scope.t >
end

val scope :
  ScopeId.t -> int -> ?entry:BlockId.t -> block_builder list -> scope_builder
(** Function to build a scope. If [entry] is not given, the first block will be
    the entry point *)

val program : program_builder -> Program.t

val from_source : string -> Program.t