%{
	open Base
%}

%token SCOPE_MARKER
%token COLON
%token COMMA
%token ENTRY
%token EXCL
%token <Base.instr_command> COMMAND
// opcodes
%token CONST
%token ADD
%token SUB
%token MUL
%token DIV
%token REM
%token UDIV
%token UREM
%token AND
%token OR
%token XOR
%token NOT
%token BRANCH
%token BRANCHIFZERO
%token BRANCHIFLESS
%token BRANCHIFULESS
%token PHI
%token CALL
%token RETURN
// end opcode
%token <string> IDENT
%token <int> REG
%token <int64> INT
%token LINE_END
%token EOF

%start <program_builder> parse
%%

parse:
| prog = search_entry { prog }

search_entry:
| LINE_END; prog = search_entry { prog }
| ENTRY; id = IDENT; LINE_END+; scopes = scopes { id, scopes }

scopes:
| scope = scope; scopes = scopes { scope :: scopes }
| EOF { [] }

scope:
| SCOPE_MARKER; id = IDENT; args = INT; entry = IDENT?; LINE_END+; blocks = blocks { id, Int64.to_int args, entry, blocks }

blocks:
| block = block; LINE_END*; blocks = blocks { block :: blocks }
| { [] }

block:
| id = IDENT; COLON; LINE_END+; instrs = instructions { id, instrs }

instructions:
| instr = instruction; LINE_END*; instrs = instructions { instr :: instrs }
| { [] }

instruction:
| CONST; d = REG; COMMA; v = INT; LINE_END { Const (d, v) }
| ADD; d = REG; COMMA; r1 = REG; COMMA; r2 = REG; LINE_END { BinOp (Add, d, r1, r2) }
| SUB; d = REG; COMMA; r1 = REG; COMMA; r2 = REG; LINE_END { BinOp (Sub, d, r1, r2) }
| MUL; d = REG; COMMA; r1 = REG; COMMA; r2 = REG; LINE_END { BinOp (Mul, d, r1, r2) }
| DIV; d = REG; COMMA; r1 = REG; COMMA; r2 = REG; LINE_END { BinOp (Div, d, r1, r2) }
| REM; d = REG; COMMA; r1 = REG; COMMA; r2 = REG; LINE_END { BinOp (Rem, d, r1, r2) }
| UDIV; d = REG; COMMA; r1 = REG; COMMA; r2 = REG; LINE_END { BinOp (Udiv, d, r1, r2) }
| UREM; d = REG; COMMA; r1 = REG; COMMA; r2 = REG; LINE_END { BinOp (Urem, d, r1, r2) }
| AND; d = REG; COMMA; r1 = REG; COMMA; r2 = REG; LINE_END { BinOp (And, d, r1, r2) }
| OR; d = REG;  COMMA;r1 = REG;  COMMA;r2 = REG; LINE_END { BinOp (Or, d, r1, r2) }
| XOR; d = REG; COMMA; r1 = REG; COMMA; r2 = REG; LINE_END { BinOp (Xor, d, r1, r2) }
| NOT; d = REG; COMMA; r = REG; LINE_END { Not (d, r) }
| BRANCH; target = IDENT; LINE_END { Branch target }
| BRANCHIFZERO; r = REG; COMMA; target = IDENT; LINE_END { BranchIfZero (r, target) }
| BRANCHIFLESS; r1 = REG; COMMA; r2 = REG; COMMA; target = IDENT; LINE_END { BranchIfLess (r1, r2, target) }
| BRANCHIFULESS; r1 = REG; COMMA; r2 = REG; COMMA; target = IDENT; LINE_END { BranchIfULess (r1, r2, target) }
| PHI; d = REG; COMMA; prev = IDENT; COMMA; r1 = REG; COMMA; r2 = REG; LINE_END { Phi (d, prev, r1, r2) }
| CALL; d = REG; COMMA; scope = IDENT; args = reg_list; LINE_END { Call (d, scope, args) }
| RETURN; r = REG; LINE_END { Return r }
| EXCL; c = COMMAND; LINE_END { Command c }

reg_list:
| COMMA; r = REG; l = reg_list { r :: l }
| { [] }