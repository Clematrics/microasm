open Base

val pp_wide_int64 : Format.formatter -> int64 -> unit
(** Pretty-print a 64bit value in hexadecimal (with a 16 width) and in decimal
    (as a signed then unsigned integer) *)

val pp_int64 : Format.formatter -> int64 -> unit
(** Pretty-print a 64bit value in hexadecimal and in decimal (as a signed then
    unsigned integer) *)

val pp_reg : Format.formatter -> int -> unit
(** Pretty-print a register name *)

val string_of_binop : bin_op -> string

val pp_list :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
(** [pp_list pp fmt list]: Pretty-print each element of [list] with the
    pretty-printer [pp] using the formatter [fmt]. The elements are enclosed by
    [\[] and [\]] *)

val pp_command :
  Format.formatter -> instr_command -> unit
(** Pretty-print a command *)

val pp_instr : Format.formatter -> instr -> unit
(** Pretty-print an instruction *)
