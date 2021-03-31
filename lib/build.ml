open Base
open Checker

module Block = struct
  type t =
    < name : BlockId.t
    ; instructions : instr array
    ; instruction : int -> instr >

  class block (name : string) (instructions : instr list) =
    object
      val name = name

      val instructions = Array.of_list instructions

      method name = name

      method instructions = instructions

      method instruction i = instructions.(i)
    end

  let make = new block
end

module Scope = struct
  type t =
    < name : ScopeId.t
    ; args_count : int
    ; block : BlockId.t -> Block.t
    ; entry_block : Block.t >

  class scope (name : ScopeId.t) (args_count : int) (entry : BlockId.t)
    (builder_list : block_builder list) =
    let blocks_assoc =
      List.map (fun (name, b) -> (name, Block.make name b)) builder_list
    in
    let blocks = Hashtbl.of_seq @@ List.to_seq blocks_assoc in
    object (self)
      val name = name

      val blocks = blocks

      val entry = entry

      val args_count = args_count

      method name = name

      method args_count = args_count

      method block name = Hashtbl.find blocks name

      method entry_block = self#block entry
    end

  let make = new scope
end

module Program = struct
  type t =
    < entry : ScopeId.t ; scope : ScopeId.t -> Scope.t ; entry_scope : Scope.t >

  class program (builder : program_builder) =
    let entry, builder_list = builder in
    let scopes_assoc =
      List.map
        (fun builder ->
          let scope_name, args, entry_opt, blocks = builder in
          ( scope_name,
            Scope.make scope_name args
              (Option.value entry_opt ~default:(fst (List.hd blocks)))
              (* List.hd should not fail, since checks against empty scopes have been made *)
              blocks ))
        builder_list
    in
    let scopes = Hashtbl.of_seq @@ List.to_seq scopes_assoc in
    object (self)
      val entry = entry

      val scopes = scopes

      method entry = entry

      method scope name = Hashtbl.find scopes name

      method entry_scope = self#scope entry
    end

  let make = new program
end

let program builder =
  let errors = check builder in
  match errors with [] -> Program.make builder | l -> raise (Not_compliant l)
