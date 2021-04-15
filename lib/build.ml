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

let scope (name : ScopeId.t) (args_count : int) ?entry
    (builder_list : block_builder list) =
  (name, args_count, entry, builder_list)

let program builder =
  let errors = check builder in
  match errors with [] -> Program.make builder | l -> raise (Not_compliant l)

let from_source filename =
  let print_position fmt (lexbuf: Lexing.lexbuf) =
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  in
  let inchn = open_in filename in
  let lexbuf = Lexing.from_channel inchn in
  let builder =
    try Parser.parse Lexer.read_token lexbuf
    with Parser.Error ->
      Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
      raise Parser.Error
  in
  program builder
