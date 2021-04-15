open Base
open Trace

module Solver (Params : sig
  val bit_width : int
end) : sig
  module RegMap : module type of Map.Make (Register)

  val bit_width : int

  val reg : Register.t -> string

  val make_const :
    Z3.context -> Register.t -> Z3.Expr.expr RegMap.t -> Z3.Expr.expr RegMap.t

  val create_var :
    Z3.context -> Z3.Expr.expr RegMap.t -> trace_instr -> Z3.Expr.expr RegMap.t
  (** Create a variable for each register seen in an instruction, event for the
      source registers *)

  val smt_of_instr :
    Z3.context -> Z3.Expr.expr RegMap.t -> trace_instr -> Z3.Expr.expr list

  val smt_converter : trace_instr list -> Z3.Model.model * Z3.Expr.expr RegMap.t

  type solver_result = NoSolution | Unknown | Satisfiable of Int64.t RegMap.t
  (* TODO: make specific type? *)

  class solver :
    object
      method solver : Z3.Solver.solver

      method add_trace : trace_instr list -> unit

      method generate_model : unit -> solver_result

      method exclude_last : unit -> unit
    end
end

module Default : module type of Solver (struct
  let bit_width = 64
end)
