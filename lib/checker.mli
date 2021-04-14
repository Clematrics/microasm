open Base

(* {1 Program checking} *)

(** Reasons for which a program could be invalid *)
type not_compliant_reason =
  | Empty_program  (** A program has no scopes but should have at least one *)
  | Bad_entry_point of ScopeId.t
      (** The entry point specified does not exist *)
  | Entry_point_has_args of ScopeId.t
      (** The entry point has args but cannot have one *)
  | Empty_scope of ScopeId.t
      (** A scope has no blocks but should have at least one *)
  | Bad_first_block of block_reference
      (** The first block specified does not exist *)
  | Duplicate_register of Register.t * instruction_reference
      (** A register is created twice in the same scope *)
  | Block_not_found of BlockId.t * instruction_reference
      (** A block referenced in the instruction cannot be found in the scope *)
  | Scope_not_found of ScopeId.t * instruction_reference
      (** A scope referenced in the instruction cannot be found in the program *)
  | Invalid_argument_count of int * int * ScopeId.t * instruction_reference
      (** Arguments given, arguments expected, scope name, ... *)

exception Not_compliant of not_compliant_reason list
(** Exception raised when a program is invalid, with the associated reasons *)

(** Check if a program being built is compliant with all rules *)
val check : program_builder -> not_compliant_reason list