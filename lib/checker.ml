open Base

type not_compliant_reason =
  | Empty_program
  | Bad_entry_point of ScopeId.t
  | Entry_point_has_args of ScopeId.t
  | Empty_scope of ScopeId.t
  | Bad_first_block of block_reference
  | Duplicate_register of Register.t * instruction_reference
  | Block_not_found of BlockId.t * instruction_reference
  | Scope_not_found of ScopeId.t * instruction_reference
  | Invalid_argument_count of int * int * ScopeId.t * instruction_reference

exception Not_compliant of not_compliant_reason list

module IdMap = Map.Make (String)
module RegSet = Set.Make (Register)

let check_instruction (reg_set, errors) scope_dictionary block_dictionary
    block_ref (no, instruction) =
  let instr_ref = (no, block_ref) in
  let new_reg, block_request, scope_request =
    match instruction with
    | Const (d, _) | BinOp (_, d, _, _) | Not (d, _) -> (Some d, None, None)
    | Branch name -> (None, Some name, None)
    | BranchIfZero (_, name) -> (None, Some name, None)
    | BranchIfLess (_, _, name) -> (None, Some name, None)
    | BranchIfULess (_, _, name) -> (None, Some name, None)
    | Phi (d, name, _, _) -> (Some d, Some name, None)
    | Call (d, name, args) -> (Some d, None, Some (name, List.length args))
    | Return _ -> (None, None, None)
    | Command _ -> (None, None, None)
  in
  let check_register_duplicate (reg_set, errors) =
    Option.fold ~none:(reg_set, errors)
      ~some:(fun reg ->
        ( RegSet.add reg reg_set,
          if RegSet.exists (( = ) reg) reg_set then
            Duplicate_register (reg, instr_ref) :: errors
          else errors ))
      new_reg
  in
  let check_block_request (reg_set, errors) =
    let errors' =
      match block_request with
      | None -> errors
      | Some block ->
          if None = IdSet.find_opt block block_dictionary then
            Block_not_found (block, instr_ref) :: errors
          else errors
    in
    (reg_set, errors')
  in
  let check_call_request (reg_set, errors) =
    let errors' =
      match scope_request with
      | None -> errors
      | Some (scope, args_passed) -> (
          try
            let args_expected = IdMap.find scope scope_dictionary in
            if args_passed = args_expected then errors
            else
              Invalid_argument_count
                (args_passed, args_expected, scope, instr_ref)
              :: errors
          with Not_found -> Scope_not_found (scope, instr_ref) :: errors)
    in
    (reg_set, errors')
  in
  check_register_duplicate (reg_set, errors)
  |> check_block_request |> check_call_request

let check_block (registers, errors) scope_dictionary block_dictionary scope_name
    block_builder =
  let block_name, instrs = block_builder in
  let block_ref = (block_name, scope_name) in
  List.fold_left
    (fun x -> check_instruction x scope_dictionary block_dictionary block_ref)
    (registers, errors)
  @@ List.mapi (fun no i -> (no, i)) instrs

let check_scope scope_dictionary scope_builder errors =
  let scope_name, args, entry, block_builders = scope_builder in
  let block_dictionary =
    IdSet.of_seq @@ List.to_seq
    @@ List.map (fun (name, _) -> name) block_builders
  in
  let check_scope_empty_and_entry errors =
    if block_builders = [] then Empty_scope scope_name :: errors
    else
      match entry with
      | None -> errors
      | Some block_name ->
          if IdSet.exists (( = ) block_name) block_dictionary then errors
          else Bad_first_block (block_name, scope_name) :: errors
  in
  let check_blocks errors =
    let identity x = x in
    let registers = RegSet.of_list (List.init args identity) in
    let _, errors =
      List.fold_left
        (fun x -> check_block x scope_dictionary block_dictionary scope_name)
        (registers, errors) block_builders
    in
    errors
  in
  check_scope_empty_and_entry errors |> check_blocks

let check (program_builder : program_builder) =
  let entry, scope_builders = program_builder in
  let scope_dictionary =
    IdMap.of_seq @@ List.to_seq
    @@ List.map (fun (name, args, _, _) -> (name, args)) scope_builders
  in

  let check_program_empty errors =
    if scope_builders = [] then Empty_program :: errors else errors
  in
  let check_program_entry errors =
    try
      match IdMap.find entry scope_dictionary with
      | 0 -> errors
      | _ -> Entry_point_has_args entry :: errors
    with Not_found -> Bad_entry_point entry :: errors
  in
  let check_scopes errors =
    List.fold_left
      (fun errors scope -> check_scope scope_dictionary scope errors)
      errors scope_builders
  in
  check_program_empty [] |> check_program_entry |> check_scopes
