open Base

let pp_wide_int64 fmt value =
  Format.fprintf fmt "%#16Lx (%#Ld|%#Lu)" value value value

let pp_int64 fmt value = Format.fprintf fmt "%#Lx (%#Ld|%#Lu)" value value value

let pp_reg fmt reg = Format.fprintf fmt "r%d" reg

let string_of_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Rem -> "Rem"
  | Udiv -> "Udiv"
  | Urem -> "Urem"
  | And -> "And"
  | Or -> "Or"
  | Xor -> "Xor"

let pp_list pp fmt list =
  let rec pp_tail fmt = function
    | [] -> Format.fprintf fmt ""
    | x :: tl -> Format.fprintf fmt ";@ %a%a" pp x pp_tail tl
  in
  match list with
  | [] -> Format.fprintf fmt "[]"
  | x :: tl ->
      Format.fprintf fmt "@[<hv 0>[@ @[<hv 2>%a%a@]@ ]@]" pp x pp_tail tl

let pp_command fmt = function
  | EnableTrace -> Format.fprintf fmt "enable trace"
  | DisableTrace -> Format.fprintf fmt "disable trace"
  | Breakpoint -> Format.fprintf fmt "breakpoint"

let pp_instr fmt = function
  | Const (d, v) ->
      Format.fprintf fmt "@[<h 0>Const@ %a,@ %a@]" pp_reg d pp_int64 v
  | BinOp (op, d, r1, r2) ->
      Format.fprintf fmt "@[<h 0>%s@ %a,@ %a,@ %a@]" (string_of_binop op) pp_reg
        d pp_reg r1 pp_reg r2
  | Not (d, r) -> Format.fprintf fmt "@[<h 0>Not@ %a,@ %a@]" pp_reg d pp_reg r
  | Branch t -> Format.fprintf fmt "@[<h 0>Branch@ %s@]" t
  | BranchIfZero (r, t) ->
      Format.fprintf fmt "@[<h 0>Branch@ %s@ if %a = 0@]" t pp_reg r
  | BranchIfLess (r1, r2, t) ->
      Format.fprintf fmt "@[<h 0>Branch@ %s@ if %a < %a (signed)@]" t pp_reg r1
        pp_reg r2
  | BranchIfULess (r1, r2, t) ->
      Format.fprintf fmt "@[<h 0>Branch@ %s@ if %a < %a (unsigned)@]" t pp_reg
        r1 pp_reg r2
  | Phi (d, t, r1, r2) ->
      Format.fprintf fmt "@[<h 0>Phi@ %a,@ %s,@ %a,@ %a@]" pp_reg d t pp_reg r1
        pp_reg r2
  | Call (d, s, args) ->
      Format.fprintf fmt "@[<h 0>Call@ %a,@ %s,@ %a@]" pp_reg d s
        (pp_list pp_reg) args
  | Return r -> Format.fprintf fmt "@[<h 0>Return@ %a@]" pp_reg r
  | Command c -> Format.fprintf fmt "@[<h 0>¤ %a@]" pp_command c
