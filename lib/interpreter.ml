open Base
open Build

exception Undefined_scope of ScopeId.t * instruction_reference
(** Undefined_scope name : indicates that the scope named `name` is missing in the program *)

exception Undefined_block of block_reference * instruction_reference
(** Undefined_block block_name, scope_name : indicates that the block named `block_named` is missing in the scope `scope_name` *)

exception Undefined_register of Register.t * instruction_reference
(** Undefined_register reg, block_name, scope_name : indicates that a register was not initialized yet in some block of some scope *)

exception Division_by_zero of instruction_reference
(** Division_by_zero block_name, scope_name *)

module Memory = Hashtbl.Make (Register)
module RegisterFile = Map.Make (Register)

type register_file = Register.t RegisterFile.t

type memory = Int64.t Memory.t

type call_stack =
  (register_file * instruction_reference * Register.t * Block.t option) list
(** File register, reference to the next instruction, register to place the result of the call into, previous block *)

type position = int * Block.t * Scope.t

type command = Continue | Next of int | StepInto | StepOut | Stop

class execution program =
  let entry_scope = program#entry_scope in
  let entry_block = entry_scope#entry_block in
  let position = (0, entry_block, entry_scope) in
  object (self)
    val program = program

    val mutable finished = false

    val mutable call_stack = []

    val mutable local_reg_file = RegisterFile.empty

    val mutable position = position

    val mutable previous_block = None

    val mutable next_global_reg = 0

    val memory = Memory.create (2 lsl 16)

    val mutable return_value = Int64.zero

    val mutable trace = []

    method private new_memory_cell () =
      let res = next_global_reg in
      next_global_reg <- next_global_reg + 1;
      res

    method private set_value local_reg value =
      let global_reg =
        try RegisterFile.find local_reg local_reg_file
        with Not_found ->
          let reg = self#new_memory_cell () in
          local_reg_file <- RegisterFile.add local_reg reg local_reg_file;
          reg
      in
      Memory.replace memory global_reg value

    method private get_value local_reg =
      try
        let global_reg = RegisterFile.find local_reg local_reg_file in
        Memory.find memory global_reg
      with Not_found -> assert false
    (* This could happen because there is no static analysis yet *)

    method private step_once () =
      let i, block, scope = position in
      position <- (i + 1, block, scope);
      match block#instruction i with
      | Const (d, value) -> self#set_value d value
      | Add (d, r1, r2) ->
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          self#set_value d (Int64.add v1 v2)
      | Sub (d, r1, r2) ->
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          self#set_value d (Int64.sub v1 v2)
      | Mul (d, r1, r2) ->
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          self#set_value d (Int64.mul v1 v2)
      | Div (d, r1, r2) -> (
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          try self#set_value d (Int64.div v1 v2)
          with _ -> raise (Division_by_zero (i, (block#name, scope#name))))
      | Rem (d, r1, r2) -> (
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          try self#set_value d (Int64.rem v1 v2)
          with _ -> raise (Division_by_zero (i, (block#name, scope#name))))
      | Udiv (d, r1, r2) -> (
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          try self#set_value d (Int64.unsigned_div v1 v2)
          with _ -> raise (Division_by_zero (i, (block#name, scope#name))))
      | Urem (d, r1, r2) -> (
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          try self#set_value d (Int64.unsigned_rem v1 v2)
          with _ -> raise (Division_by_zero (i, (block#name, scope#name))))
      | Not (d, r) ->
          let v = self#get_value r in
          self#set_value d (Int64.lognot v)
      | And (d, r1, r2) ->
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          self#set_value d (Int64.logand v1 v2)
      | Or (d, r1, r2) ->
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          self#set_value d (Int64.logor v1 v2)
      | Xor (d, r1, r2) ->
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          self#set_value d (Int64.logxor v1 v2)
      | Branch next_block ->
          previous_block <- Some block;
          position <- (0, scope#block next_block, scope)
      | BranchIfZero (r, next_block) ->
          let v = self#get_value r in
          if Int64.equal v Int64.zero then (
            previous_block <- Some block;
            position <- (0, scope#block next_block, scope))
      | BranchIfLess (r1, r2, next_block) ->
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          if Int64.compare v1 v2 < 0 then (
            previous_block <- Some block;
            position <- (0, scope#block next_block, scope))
      | BranchIfULess (r1, r2, next_block) ->
          let v1 = self#get_value r1 and v2 = self#get_value r2 in
          if Int64.unsigned_compare v1 v2 < 0 then (
            previous_block <- Some block;
            position <- (0, scope#block next_block, scope))
      | Phi (d, name, r1, r2) ->
          let v =
            match previous_block with
            | Some scope when scope#name = name -> self#get_value r1
            | _ -> self#get_value r2
          in
          self#set_value d v
      | Call (d, name, args) ->
          (* Local copy of args *)
          let args_value = List.map (fun r -> self#get_value r) args in
          (* Saving local register file and caller status *)
          call_stack <-
            ( local_reg_file,
              (i + 1, (block#name, scope#name)),
              d,
              previous_block )
            :: call_stack;
          (* Creating a new local register file for the callee *)
          local_reg_file <- RegisterFile.empty;
          List.iteri (fun i v -> self#set_value i v) args_value;
          let new_scope = program#scope name in
          position <- (0, new_scope#entry_block, new_scope);
          previous_block <- None
      | Return r -> (
          let ret_value = self#get_value r in
          match call_stack with
          | [] ->
              return_value <- ret_value;
              self#execute Stop
          | (old_reg_file, instr_ref, d, prev_block) :: tail ->
              call_stack <- tail;
              local_reg_file <- old_reg_file;
              let i, (block_name, scope_name) = instr_ref in
              let new_scope = program#scope scope_name in
              position <- (i, new_scope#block block_name, new_scope);
              previous_block <- prev_block;
              self#set_value d ret_value)

    method print fmt =
      let int64 fmt value = Format.fprintf fmt "%Ld (%Lx)" value value in
      Format.fprintf fmt "@[<h>Finished:@ %B;@ Return value:@ %a@]@," finished
        int64 return_value;
      (* Finished + return value *)
      let i, b, s = position in
      Format.fprintf fmt "@[<h>Position:@ (%d@ : %s@ : %s)@]@," i b#name s#name;
      (* Print pos *)
      let reg fmt (reg, glob, value) =
        Format.fprintf fmt "@[<h>@<8>%u@ @<8>%u@ @<30>%a@]" reg glob int64 value
      in
      let regs fmt =
        RegisterFile.iter
          (fun loc glob ->
            let value = self#get_value loc in
            Format.fprintf fmt "%a@," reg (loc, glob, value))
          local_reg_file
      in
      Format.fprintf fmt "@[Registers (local id, global id, value):@ %t@]@,"
        regs;
      (* Print local_reg global_reg value *)
      let pp_option fmt = function
        | None -> Format.fprintf fmt "None"
        | Some b -> Format.fprintf fmt "%s" b#name
      in
      let frame fmt (_, (i, (b, s)), ret, prev) =
        Format.fprintf fmt
          "@[<h>Next instruction:@ %u,@ %s,@ %s;@ Return register:@ %u;@ \
           Previous block:@ %a@]"
          i b s ret pp_option prev
      in
      let rec pp_stack limit fmt = function
        | [] -> Format.fprintf fmt "End of call stack"
        | _ when limit = 0 -> Format.fprintf fmt "       ...       "
        | hd :: tl ->
            Format.fprintf fmt "%a@,%a" frame hd (pp_stack (limit - 1)) tl
      in
      Format.fprintf fmt "@[<v>%a@]@\n" (pp_stack 4) call_stack
    (* Print stack up to ... *)

    method printf () = self#print Format.std_formatter

    method execute command =
      if finished then ()
      else
        match command with
        | Continue ->
            while not finished do
              self#step_once ()
            done
        | Next n ->
            let rec loop i =
              self#step_once ();
              if i > 0 && not finished then loop (i - 1)
            in
            loop n
        | StepInto -> () (* TODO: *)
        | StepOut -> () (* TODO: *)
        | Stop -> finished <- true

    method result () =
      if finished then return_value
      else raise (Invalid_argument "Execution has not terminated yet")
  end
