open Base
open Build
open Trace

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

class execution ?(enable_trace = false) program =
  let entry_scope = program#entry_scope in
  let entry_block = entry_scope#entry_block in
  let position = (0, entry_block, entry_scope) in
  object (self)
    val program = program

    val mutable finished = false

    val mutable interrupted = false

    val mutable call_stack = []

    val mutable call_stack_height = 0

    val mutable local_reg_file = RegisterFile.empty

    val mutable position = position

    val mutable previous_block = None

    val mutable next_global_reg = 0

    val memory = Memory.create (2 lsl 16)

    val mutable return_value = Int64.zero

    val mutable enable_trace = enable_trace

    val mutable trace = []

    method trace () = linearize @@ List.rev trace
    (* Trace is stored in reverse order *)

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
      Memory.replace memory global_reg value;
      global_reg

    method private get_value local_reg =
      let global_reg = RegisterFile.find local_reg local_reg_file in
      (Memory.find memory global_reg, global_reg)
    (** @raise Not_found
    This happen when tracing for Phi instructions,
    when one of the registers is not yet created in memory.
    It could also happen because there is no static analysis yet *)

    method private push_stack call =
      call_stack <- call :: call_stack;
      call_stack_height <- call_stack_height + 1

    method private pop_stack () =
      match call_stack with
      | [] -> None
      | hd :: tl ->
          call_stack <- tl;
          call_stack_height <- call_stack_height - 1;
          Some hd

    method private current_instr () =
      let i, block, _ = position in
      block#instruction i

    method private step_once () =
      let i, block, scope = position in
      let instr = self#current_instr () in
      position <- (i + 1, block, scope);
      match instr with
      | Command `EnableTrace -> self#execute EnableTrace
      | Command `DisableTrace -> self#execute DisableTrace
      | Command `Breakpoint -> interrupted <- true
      | _ ->
          let instr_trace =
            match instr with
            | Const (d, value) ->
                let grd = self#set_value d value in
                TConst (grd, value)
            | BinOp (op, d, r1, r2) ->
                let v1, gr1 = self#get_value r1
                and v2, gr2 = self#get_value r2 in
                let v =
                  match op with
                  | Add -> Int64.add v1 v2
                  | Sub -> Int64.sub v1 v2
                  | Mul -> Int64.mul v1 v2
                  | Div -> (
                      try Int64.div v1 v2
                      with _ ->
                        raise (Division_by_zero (i, (block#name, scope#name))))
                  | Rem -> (
                      try Int64.rem v1 v2
                      with _ ->
                        raise (Division_by_zero (i, (block#name, scope#name))))
                  | Udiv -> (
                      try Int64.unsigned_div v1 v2
                      with _ ->
                        raise (Division_by_zero (i, (block#name, scope#name))))
                  | Urem -> (
                      try Int64.unsigned_rem v1 v2
                      with _ ->
                        raise (Division_by_zero (i, (block#name, scope#name))))
                  | And -> Int64.logand v1 v2
                  | Or -> Int64.logor v1 v2
                  | Xor -> Int64.logxor v1 v2
                in
                let grd = self#set_value d v in
                TBinOp (op, grd, gr1, gr2)
            | Not (d, r) ->
                let v, gr1 = self#get_value r in
                let grd = self#set_value d (Int64.lognot v) in
                TNot (grd, gr1)
            | Branch next_block ->
                previous_block <- Some block;
                position <- (0, scope#block next_block, scope);
                TBranch next_block
            | BranchIfZero (r, next_block) ->
                let v, gr = self#get_value r in
                let condition = Int64.equal v Int64.zero in
                if condition then (
                  previous_block <- Some block;
                  position <- (0, scope#block next_block, scope));
                TBranchIfZero
                  (gr, next_block, if condition then Taken else NotTaken)
            | BranchIfLess (r1, r2, next_block) ->
                let v1, gr1 = self#get_value r1
                and v2, gr2 = self#get_value r2 in
                let condition = Int64.compare v1 v2 < 0 in
                if condition then (
                  previous_block <- Some block;
                  position <- (0, scope#block next_block, scope));
                TBranchIfLess
                  (gr1, gr2, next_block, if condition then Taken else NotTaken)
            | BranchIfULess (r1, r2, next_block) ->
                let v1, gr1 = self#get_value r1
                and v2, gr2 = self#get_value r2 in
                let condition = Int64.unsigned_compare v1 v2 < 0 in
                if condition then (
                  previous_block <- Some block;
                  position <- (0, scope#block next_block, scope));
                TBranchIfULess
                  (gr1, gr2, next_block, if condition then Taken else NotTaken)
            | Phi (d, name, r1, r2) ->
                let take_first =
                  (function
                    | Some scope when scope#name = name -> true | _ -> false)
                    previous_block
                in
                let get_value_alt r =
                  try Found (self#get_value r) with Not_found -> NotFound r
                in
                let extract_value = function
                  | Found (v, _) -> v
                  | NotFound r ->
                      raise
                        (Undefined_register (r, (i, (block#name, scope#name))))
                in
                let extract_greg = function
                  | Found (_, gr) -> Some gr
                  | _ -> None
                in
                let opt1 = get_value_alt r1 and opt2 = get_value_alt r2 in
                let grd =
                  self#set_value d
                    (if take_first then extract_value opt1
                    else extract_value opt2)
                in
                TPhi
                  ( grd,
                    name,
                    extract_greg opt1,
                    extract_greg opt2,
                    if take_first then FirstSelected else OtherSelected )
            | Call (d, name, args) ->
                (* Local copy of args *)
                let args_value, grs =
                  List.split @@ List.map (fun r -> self#get_value r) args
                in
                (* Saving local register file and caller status *)
                self#push_stack
                  ( local_reg_file,
                    (i + 1, (block#name, scope#name)),
                    d,
                    previous_block );
                (* Creating a new local register file for the callee *)
                local_reg_file <- RegisterFile.empty;
                let grds =
                  List.mapi (fun i v -> self#set_value i v) args_value
                in
                let new_scope = program#scope name in
                position <- (0, new_scope#entry_block, new_scope);
                previous_block <- None;
                TCall (grds, name, grs)
            | Return r -> (
                let ret_value, gr1 = self#get_value r in
                match self#pop_stack () with
                | None ->
                    return_value <- ret_value;
                    self#execute Stop;
                    TStop gr1
                | Some (old_reg_file, instr_ref, d, prev_block) ->
                    RegisterFile.iter
                      (fun _ gr -> Memory.remove memory gr)
                      local_reg_file;
                    (* Deleting global registers linked to this scope *)
                    local_reg_file <- old_reg_file;
                    let i, (block_name, scope_name) = instr_ref in
                    let new_scope = program#scope scope_name in
                    position <- (i, new_scope#block block_name, new_scope);
                    previous_block <- prev_block;
                    let grd = self#set_value d ret_value in
                    TReturn (grd, gr1))
            | Command _ -> assert false
            (* This should not be reachable, as it is handled by execute *)
          in
          if enable_trace then trace <- instr_trace :: trace

    method print fmt =
      Format.pp_open_vbox fmt 0;
      Format.fprintf fmt "@[<h 0>Finished:@ %B;@ Return value:@ %a@]@ " finished
        pp_int64 return_value;
      (* Finished + return value *)
      (* Print local_reg global_reg value *)
      let pp_option fmt = function
        | None -> Format.fprintf fmt "None"
        | Some b -> Format.fprintf fmt "%s" b#name
      in
      let frame fmt (_, (i, (b, caller)), ret, prev) =
        Format.fprintf fmt
          "@[<h 0>%s@ at %s:%u@ into %u@ (previous block:@ %a)@]" caller b
          (i - 1) ret pp_option prev
      in
      let rec pp_stack limit fmt = function
        | [] -> Format.fprintf fmt ""
        | _ when limit = 0 -> Format.fprintf fmt "@ ..."
        | hd :: tl ->
            Format.fprintf fmt "@ %a%a" frame hd (pp_stack (limit - 1)) tl
      in
      Format.fprintf fmt "@[<v 4>Call stack:%a@]@ " (pp_stack 4) call_stack;
      (* Print stack up to ... *)
      let reg fmt (reg, glob, value) =
        Format.fprintf fmt "@[<h 0>%8u@ -> %8u@ -> %a@]" reg glob pp_wide_int64
          value
      in
      let regs fmt =
        RegisterFile.iter
          (fun loc glob ->
            let value, _ = self#get_value loc in
            Format.fprintf fmt "@ %a" reg (loc, glob, value))
          local_reg_file
      in
      Format.fprintf fmt
        "@[<v 4>Registers (local id, global id, value (s|u)):%t@]@ " regs;
      let i, block, scope = position in
      let pp_code fmt view_size =
        let block_size = Array.length block#instructions in
        let from = max (i - view_size) 0
        and to_ = min (i + view_size) (block_size - 1) in
        if from > 0 then Format.fprintf fmt "     ...@ ";
        for j = from to to_ do
          if i = j then
            Format.fprintf fmt "--> %a@ " pp_instr (block#instruction j)
          else Format.fprintf fmt "    %a@ " pp_instr (block#instruction j)
        done;
        if to_ < block_size - 1 then Format.fprintf fmt "     ...@ "
      in
      Format.fprintf fmt "@[<v 4>%s:@ %s:@ %a@]" scope#name block#name pp_code 4;
      (* Print position and code *)
      Format.pp_close_box fmt ()

    method printf () = self#print Format.std_formatter

    method execute command =
      if finished then ()
      else
        match command with
        | Continue ->
            interrupted <- false;
            while (not finished) && not interrupted do
              self#step_once ()
            done
        | Next n ->
            interrupted <- false;
            let rec loop i =
              if i > 0 && (not finished) && not interrupted then (
                (match self#current_instr () with
                | Call _ ->
                    self#step_once ();
                    self#execute StepOut
                | _ -> self#step_once ());
                loop (i - 1))
            in
            loop n
        | Step -> self#execute (Next 1)
        | StepInto -> self#step_once ()
        | StepOut ->
            interrupted <- false;
            let stack_height = List.length call_stack in
            while
              (not finished) && (not interrupted)
              && call_stack_height >= stack_height
            do
              self#step_once ()
            done
        | Stop -> finished <- true
        | EnableTrace -> enable_trace <- true
        | DisableTrace -> enable_trace <- false

    method result () =
      if finished then return_value
      else raise (Invalid_argument "Execution has not terminated yet")
  end
