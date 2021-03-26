type register = Int.t
(** A register is just an integer identifier for a memory unit
*)

module Context = Map.Make (Int)
module RegSet = Set.Make (Int)

type context = int64 Context.t
(** A context is a mapping between registers and their value
*)

type program = string * (string, scope) Hashtbl.t
(** program : entry_point, scopes
    A program is a set of scopes with an entry_point, which indicates the name of the first scope to be executed.
    This scope must have no arguments.
*)

(** scope : args_count, first_block, blocks
    A scope is a function-like structure with a number of parameters and blocks, where registers are isolated from other scopes.
    The parameters fills the first registers of the scope, and all other registers are non-existant.
    The execution starts at first_block
*)
and scope = int * string * (string, block) Hashtbl.t
(** *)

and block = instr list
(** A block is a list of instructions that uses the registers of the scope it is contained in.
*)

(** An SSA-like instruction
*)
and instr =
  | Const of register * Int64.t  (** Const d, c : loads c into d *)
  | Add of register * register * register
      (** Add d, r1, r2 : addition of integers : d = r1 + r2 *)
  | Sub of register * register * register
      (** Sub d, r1, r2 : subtraction of integers : d = r1 - r2 *)
  | Mul of register * register * register
      (** Mul d, r1, r2 : multiplication of integers : d = (r1 * r2) & 0b111...111
          The bits that do not fit in the memory cell are ignored *)
  | Div of register * register * register
      (** Div d, r1, r2 : signed division of integers : d = r1 // r2.
          If r2 = 0, the program halts and reports an error *)
  | Rem of register * register * register
      (** Rem d, r1, r2 : signed integer remainder : d = r1 mod r2.
          If r2 = 0, the program halts and reports an error *)
  | Udiv of register * register * register
      (** Div d, r1, r2 : unsigned division of integers : d = r1 // r2.
          If r2 = 0, the program halts and reports an error *)
  | Urem of register * register * register
      (** Rem d, r1, r2 : unsigned integer remainder : d = r1 mod r2.
          If r2 = 0, the program halts and reports an error *)
  | Not of register * register  (** Not d, r : bitwise negation *)
  | And of register * register * register  (** And d, r1, r2 : bitwise and *)
  | Or of register * register * register  (** Or d, r1, r2 : bitwise or *)
  | Xor of register * register * register
      (** Xor d, r1, r2 : bitwise exclusive or *)
  | Branch of string
      (** Branch target : jumps to the block `target`. Puts the current block name as the previous block encountered *)
  | BranchIfZero of register * string
      (** BranchIfZero r, target : jumps to the block `target` if r contains 0, otherwise, continues to the next instruction.
          Puts the current block name as the previous block encountered *)
  | BranchIfLess of register * register * string
      (** BranchIfLess r1, r2, target : jumps to the block `target` if r1 < r2, where r1 and r2 are interpreted as signed numbers.
          Otherwise, continues to the next instruction.
          Puts the current block number as the previous block encountered *)
  | BranchIfULess of register * register * string
      (** BranchIfLess r1, r2, target : jumps to the block `target` if r1 < r2, where r1 and r2 are interpreted as unsigned numbers.
          Otherwise, continues to the next instruction.
          Puts the current block number as the previous block encountered *)
  | Phi of register * string * register * register
      (** Phi d, b1, r1, r2 : puts r1 into d if the execution came from block b, otherwise, puts r2 into d *)
  | Call of register * string * register list
      (** Call d, name, [r1, ...] : calls the scope with id `name`, passing registers r1, ... as arguments.
          The result of the call is put in d *)
  | Return of register
      (** Return r : returns the content of r to the calling scope.
          A block which does not end with a branch or a return instruction returns implicitly zero *)

let new_program entry_point (scope_list : (string * scope) list) : program =
  (entry_point, Hashtbl.of_seq (List.to_seq scope_list))

let new_scope (name : string) args (block_list : (string * block) list) :
    string * scope =
  let scope = Hashtbl.of_seq (List.to_seq block_list) in
  let first_block, _ = List.hd block_list in
  (name, (args, first_block, scope))

let new_block (name : string) instr_list : string * block = (name, instr_list)

type not_compliant_reason =
  | Bad_entry_point of string
  | Entry_point_has_args of string
  | Bad_first_block of string * string  (** Bad_first_block (block, scope) *)
  | Duplicate_register of register * string * string
      (** Duplicate_register (register, block, scope) *)
  | Block_not_found of string * string * string
      (** Block_not_found (block_requested, in_block, in_scope) *)
  | Scope_not_found of string * string * string
      (** Scope_not_found (scope_requested, in_block, in_scope) *)
  | Invalid_argument_count of int * int * string * string * string
      (** Invalid_argument_count (args_count_passed, args_count_expected, scope_requested, in_block, in_scope) *)

exception Not_compliant of not_compliant_reason

(** Checks if a program complies with all rules. This does not prevent runtime errors from happening *)
let check (entry_point, scopes) =
  (* Entry point exists *)
  let args, _, _ =
    try Hashtbl.find scopes entry_point
    with Not_found -> raise (Not_compliant (Bad_entry_point entry_point))
  in
  (* Entry point has no args *)
  if args != 0 then raise (Not_compliant (Entry_point_has_args entry_point));
  (* For all scopes *)
  Hashtbl.iter
    (fun scope_name (args, first, blocks) ->
      let registers = RegSet.of_list (List.init args (fun n -> n)) in
      (* First block exists *)
      let _ =
        try Hashtbl.find blocks first
        with Not_found ->
          raise (Not_compliant (Bad_first_block (first, scope_name)))
      in
      (* For all blocks *)
      let _ = Hashtbl.fold
        (fun block_name block registers ->
          (* For all instructions *)
          List.fold_left
            (fun registers instr ->
              let new_reg, block_request, scope_request =
                match instr with
                | Const (d, _) -> (Some d, None, None)
                | Add (d, _, _) -> (Some d, None, None)
                | Sub (d, _, _) -> (Some d, None, None)
                | Mul (d, _, _) -> (Some d, None, None)
                | Div (d, _, _) -> (Some d, None, None)
                | Rem (d, _, _) -> (Some d, None, None)
                | Udiv (d, _, _) -> (Some d, None, None)
                | Urem (d, _, _) -> (Some d, None, None)
                | Not (d, _) -> (Some d, None, None)
                | And (d, _, _) -> (Some d, None, None)
                | Or (d, _, _) -> (Some d, None, None)
                | Xor (d, _, _) -> (Some d, None, None)
                | Branch name -> (None, Some name, None)
                | BranchIfZero (_, name) -> (None, Some name, None)
                | BranchIfLess (_, _, name) -> (None, Some name, None)
                | BranchIfULess (_, _, name) -> (None, Some name, None)
                | Phi (d, name, _, _) -> (Some d, Some name, None)
                | Call (d, name, args) ->
                    (Some d, None, Some (name, List.length args))
                | Return _ -> (None, None, None)
              in
              let registers' =
                Option.fold ~none:registers
                  ~some:(fun reg ->
                    if RegSet.exists (( = ) reg) registers then
                      raise
                        (Not_compliant
                           (Duplicate_register (reg, block_name, scope_name)));
                    RegSet.add reg registers)
                  new_reg
              in
              Option.iter
                (fun name ->
                  if None = Hashtbl.find_opt blocks name then
                    raise
                      (Not_compliant
                         (Block_not_found (name, block_name, scope_name))))
                block_request;
              Option.iter
                (fun (name, count) ->
                  match Hashtbl.find_opt scopes name with
                  | None ->
                      raise
                        (Not_compliant
                           (Scope_not_found (name, block_name, scope_name)))
                  | Some (args, _, _) when count != args ->
                      raise
                        (Not_compliant
                           (Invalid_argument_count
                              (count, args, name, block_name, scope_name)))
                  | _ -> ())
                scope_request;
              registers')
            registers block)
        blocks registers in ())
    scopes

exception Undefined_scope of string
(** Undefined_scope name : indicates that the scope named `name` is missing in the program *)

exception Undefined_block of string * string
(** Undefined_block block_name, scope_name : indicates that the block named `block_named` is missing in the scope `scope_name` *)

exception Undefined_register of register * string * string
(** Undefined_register reg, block_name, scope_name : indicates that a register was not initialized yet in some block of some scope *)

exception Division_by_zero of string * string
(** Division_by_zero block_name, scope_name *)

(* TODO: add dump info *)

let run ((entry_point, scopes) : program) =
  try
    let get_scope name =
      match Hashtbl.find_opt scopes name with
      | None -> raise (Undefined_scope name)
      | Some scope -> scope
    in
    let rec call_scope scope_name context =
      let _, first_block, blocks = get_scope scope_name in
      let get_block name =
        match Hashtbl.find_opt blocks name with
        | None -> raise (Undefined_block (name, scope_name))
        | Some block -> block
      in
      let rec execute instrs context current_block last_block =
        let get_value register =
          match Context.find_opt register context with
          | None ->
              raise (Undefined_register (register, current_block, scope_name))
          | Some value -> value
        in
        match instrs with
        | [] -> Int64.zero
        | Const (d, value) :: instrs ->
            let context' = Context.add d value context in
            execute instrs context' current_block last_block
        | Add (d, r1, r2) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            let context' = Context.add d (Int64.add v1 v2) context in
            execute instrs context' current_block last_block
        | Sub (d, r1, r2) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            let context' = Context.add d (Int64.sub v1 v2) context in
            execute instrs context' current_block last_block
        | Mul (d, r1, r2) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            let context' = Context.add d (Int64.mul v1 v2) context in
            execute instrs context' current_block last_block
        | Div (d, r1, r2) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            let v =
              try Int64.div v1 v2
              with _ -> raise (Division_by_zero (current_block, scope_name))
            in
            let context' = Context.add d v context in
            execute instrs context' current_block last_block
        | Rem (d, r1, r2) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            let v =
              try Int64.rem v1 v2
              with _ -> raise (Division_by_zero (current_block, scope_name))
            in
            let context' = Context.add d v context in
            execute instrs context' current_block last_block
        | Udiv (d, r1, r2) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            let v =
              try Int64.unsigned_div v1 v2
              with _ -> raise (Division_by_zero (current_block, scope_name))
            in
            let context' = Context.add d v context in
            execute instrs context' current_block last_block
        | Urem (d, r1, r2) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            let v =
              try Int64.unsigned_rem v1 v2
              with _ -> raise (Division_by_zero (current_block, scope_name))
            in
            let context' = Context.add d v context in
            execute instrs context' current_block last_block
        | Not (d, r) :: instrs ->
            let v = get_value r in
            let context' = Context.add d (Int64.lognot v) context in
            execute instrs context' current_block last_block
        | And (d, r1, r2) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            let context' = Context.add d (Int64.logand v1 v2) context in
            execute instrs context' current_block last_block
        | Or (d, r1, r2) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            let context' = Context.add d (Int64.logor v1 v2) context in
            execute instrs context' current_block last_block
        | Xor (d, r1, r2) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            let context' = Context.add d (Int64.logxor v1 v2) context in
            execute instrs context' current_block last_block
        | Branch next_block :: _ ->
            let next_instrs = get_block next_block in
            execute next_instrs context next_block (Some current_block)
        | BranchIfZero (r, next_block) :: instrs ->
            let v = get_value r in
            if Int64.equal v Int64.zero then
              let next_instrs = get_block next_block in
              execute next_instrs context next_block (Some current_block)
            else execute instrs context current_block last_block
        | BranchIfLess (r1, r2, next_block) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            if Int64.compare v1 v2 < 0 then
              let next_instrs = get_block next_block in
              execute next_instrs context next_block (Some current_block)
            else execute instrs context current_block last_block
        | BranchIfULess (r1, r2, next_block) :: instrs ->
            let v1 = get_value r1 and v2 = get_value r2 in
            if Int64.unsigned_compare v1 v2 < 0 then
              let next_instrs = get_block next_block in
              execute next_instrs context next_block (Some current_block)
            else execute instrs context current_block last_block
        | Phi (d, name, r1, r2) :: instrs ->
            let v =
              if Some name = last_block then get_value r1 else get_value r2
            in
            let context' = Context.add d v context in
            execute instrs context' current_block last_block
        | Call (d, name, args) :: instrs ->
            let child_context =
              Context.of_seq
                (List.to_seq (List.mapi (fun i r -> (i, get_value r)) args))
            in
            let v = call_scope name child_context in
            let context' = Context.add d v context in
            execute instrs context' current_block last_block
        | Return r :: _ -> get_value r
      in
      execute (get_block first_block) context first_block None
    in
    let result = call_scope entry_point Context.empty in
    Printf.printf "Program returned %s\n" (Int64.to_string result)
  with
  | Undefined_scope name ->
      Printf.printf
        "Attempting to call a scope %s which is missing in the program\n" name
  | Undefined_block (block, scope) ->
      Printf.printf "Attempting to reach an inexistant block %s in scope %s\n"
        block scope
  | Undefined_register (reg, block, scope) ->
      Printf.printf
        "Attempting to read an uninitialized register %d in block %s and scope \
         %s\n"
        reg block scope

let value = Int64.of_int

let recursive_factorial =
  new_scope "recursive_factorial" 1
    [
      new_block "check_base_case"
        [
          Const (1, value 2);
          Const (2, value 1);
          BranchIfLess (0, 1, "return");
          Branch "main_case";
        ];
      new_block "main_case"
        [
          Sub (3, 0, 2);
          Call (4, "recursive_factorial", [ 3 ]);
          Mul (5, 4, 0);
          Branch "return";
        ];
      new_block "return" [ Phi (6, "main_case", 5, 2); Return 6 ];
    ]

let loop_factorial =
  new_scope "loop_factorial" 1
    [
      new_block "init_loop" [ Const (1, value 1); Branch "loop_body" ];
      new_block "loop_body"
        [
          Phi (2, "init_loop", 1, 4);
          Phi (3, "init_loop", 0, 5);
          Mul (4, 2, 3);
          Sub (5, 3, 1);
          BranchIfLess (5, 1, "return");
          Branch "loop_body";
        ];
      new_block "return" [ Return 2 ];
    ]

let main =
  new_scope "main" 0
    [
      new_block ""
        [ Const (0, value 5); Call (1, "loop_factorial", [ 0 ]); Return 1 ];
    ]

let factorial_test =
  new_program "main" [ main; recursive_factorial; loop_factorial ]
