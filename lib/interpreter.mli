open Base
open Build
open Trace

exception Undefined_scope of ScopeId.t * instruction_reference
(** Undefined_scope name : indicates that the scope named `name` is missing in
    the program *)

exception Undefined_block of block_reference * instruction_reference
(** Undefined_block block_name, scope_name : indicates that the block named
    `block_named` is missing in the scope `scope_name` *)

exception Undefined_register of Register.t * instruction_reference
(** Undefined_register reg, block_name, scope_name : indicates that a register
    was not initialized yet in some block of some scope *)

exception Division_by_zero of instruction_reference
(** Division_by_zero block_name, scope_name *)

module Memory : module type of Hashtbl.Make (Register)
module RegisterFile : module type of Map.Make (Register)

type register_file = Register.t RegisterFile.t

type memory = Int64.t Memory.t

type call_stack =
  (register_file * instruction_reference * Register.t * Block.t option) list
(** File register, reference to the next instruction, register to place the
    result of the call into, previous block *)

type position = int * Block.t * Scope.t

type register_found = Found of (Int64.t * Register.t) | NotFound of Register.t

type command =
  | Continue
  | Next of int
  | Step
  | StepInto
  | StepOut
  | Stop
  | EnableTrace
  | DisableTrace

class execution : ?enable_trace:bool -> Program.t -> object
	method trace : unit -> trace_instr list
	method print : Format.formatter -> unit
	method printf : unit -> unit
	method execute : command -> unit
	method result : unit -> int64
end
