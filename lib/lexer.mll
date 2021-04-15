{
open Lexing
open Parser
open Base

exception SyntaxError of string

(* let entry_kw = "entry" *)

(* 
type opcode =
Const | Add | Sub | Mul | Div
| Rem | Udiv | Urem | And | Or
| Xor | Not | Branch | BranchIfZero
| BranchIfLess | BranchIfULess | Phi
| Call | Return | Command of command *)

let table = Hashtbl.create 20

let () =
	List.iter (fun (kwd, tok) -> Hashtbl.add table kwd tok)
	[
		"entry", ENTRY;
		"trace", COMMAND EnableTrace;
		"untrace", COMMAND DisableTrace;
		"break", COMMAND Breakpoint;
		"const", CONST;
		"add", ADD;
		"sub", SUB;
		"mul", MUL;
		"div", DIV;
		"rem", REM;
		"udiv", UDIV ;
		"urem", UREM ;
		"and", AND ;
		"or", OR ;
		"xor", XOR ;
		"not", NOT ;
		"branch", BRANCH ;
		"bl", BRANCHIFLESS ;
		"bul", BRANCHIFULESS ;
		"phi", PHI ;
		"call", CALL ;
		"ret", RETURN ;
	]

let next_line lexbuf =
	let pos = lexbuf.lex_curr_p in
	lexbuf.lex_curr_p <- {
		pos with pos_bol = lexbuf.lex_curr_pos;
		pos_lnum = pos.pos_lnum + 1
	}
}

let whitespaces = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let natural_int = digit+
let decimal_int = '-'? ('_')* digit (digit | '_')*

let hex_digit = ['a'-'f' 'A'-'F'] | digit
let hex_int = "0x" ('_')* hex_digit (hex_digit | '_')*

let bin_digit = '0' | '1'
let bin_int = "0b" ('_')* bin_digit (bin_digit | '_')*

(* identifiers cannot start by a digit, and must contain at least one letter *)
let ident = ('_')* digit* alpha (alpha | digit | '_')*

let comment = "//"
let multiline_comm_start = "/*"
let multiline_comm_end = "*/"

rule read_token = parse
| '@' { SCOPE_MARKER }
| ':' { COLON }
| ',' { COMMA }
| '!' { EXCL }
| whitespaces { read_token lexbuf }
| ident { let ident = Lexing.lexeme lexbuf in try Hashtbl.find table ident with Not_found -> IDENT (ident) }
| natural_int { NAT (int_of_string (Lexing.lexeme lexbuf)) } (* could be bad *)
| decimal_int { INT (Int64.of_string (Lexing.lexeme lexbuf)) }
| hex_int { INT (Int64.of_string (Lexing.lexeme lexbuf)) }
| bin_int { INT (Int64.of_string (Lexing.lexeme lexbuf)) }
| newline { next_line lexbuf; LINE_END }
| comment { comment lexbuf }
| multiline_comm_start { multiline_comment lexbuf }
| eof { EOF }
and comment = parse
| newline { next_line lexbuf; LINE_END }
| eof { EOF }
| _ { comment lexbuf }
and multiline_comment = parse
| multiline_comm_end { read_token lexbuf }
| eof { raise (SyntaxError "Reached end of file inside a multiline comment") }
| _ { multiline_comment lexbuf }
