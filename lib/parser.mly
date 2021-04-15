%{
	open Base
%}

%token SCOPE_MARKER
%token COLON
%token COMMA
%token ENTRY
%token EXCL
%token <Base.instr_command> COMMAND
// %token <opcode> OPCODE
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
%token <int> NAT
%token <int64> INT
%token LINE_END
%token EOF

%start <program_builder> parse
%%

parse:
| EOF { raise (Checker.Not_compliant [Checker.Empty_program]) }
| ENTRY; id = IDENT; LINE_END; scopes = scopes { id, scopes }

scopes:
| EOF { [] }
| SCOPE_MARKER; id = IDENT; args = NAT; sep; blocks = blocks; scopes = scopes { (id, args, None, blocks) :: scopes }

blocks:
| LINE_END { [] }
| id = IDENT; COLON; sep; instr = instructions; blocks = blocks { (id, instr) :: blocks }

instructions:
| LINE_END { [] }
| CONST; d = NAT; COMMA; v = INT; sep; instr = instructions { (Const (d, v)) :: instr }
| ADD; d = NAT; COMMA; r1 = NAT; COMMA; r2 = NAT; sep; instr = instructions { (BinOp (Add, d, r1, r2)) :: instr }
| SUB; d = NAT; COMMA; r1 = NAT; COMMA; r2 = NAT; sep; instr = instructions { (BinOp (Sub, d, r1, r2)) :: instr }
| MUL; d = NAT; COMMA; r1 = NAT; COMMA; r2 = NAT; sep; instr = instructions { (BinOp (Mul, d, r1, r2)) :: instr }
| DIV; d = NAT; COMMA; r1 = NAT; COMMA; r2 = NAT; sep; instr = instructions { (BinOp (Div, d, r1, r2)) :: instr }
| REM; d = NAT; COMMA; r1 = NAT; COMMA; r2 = NAT; sep; instr = instructions { (BinOp (Rem, d, r1, r2)) :: instr }
| UDIV; d = NAT; COMMA; r1 = NAT; COMMA; r2 = NAT; sep; instr = instructions { (BinOp (Udiv, d, r1, r2)) :: instr }
| UREM; d = NAT; COMMA; r1 = NAT; COMMA; r2 = NAT; sep; instr = instructions { (BinOp (Urem, d, r1, r2)) :: instr }
| AND; d = NAT; COMMA; r1 = NAT; COMMA; r2 = NAT; sep; instr = instructions { (BinOp (And, d, r1, r2)) :: instr }
| OR; d = NAT;  COMMA;r1 = NAT;  COMMA;r2 = NAT; sep; instr = instructions { (BinOp (Or, d, r1, r2)) :: instr }
| XOR; d = NAT; COMMA; r1 = NAT; COMMA; r2 = NAT; sep; instr = instructions { (BinOp (Xor, d, r1, r2)) :: instr }
| NOT; d = NAT; COMMA; r = NAT; sep; instr = instructions { (Not (d, r)) :: instr }
| BRANCH; target = IDENT; sep; instr = instructions { (Branch target) :: instr }
| BRANCHIFZERO; r = NAT; COMMA; target = IDENT; sep; instr = instructions { (BranchIfZero (r, target)) :: instr }
| BRANCHIFLESS; r1 = NAT; COMMA; r2 = NAT; COMMA; target = IDENT; sep; instr = instructions { (BranchIfLess (r1, r2, target)) :: instr }
| BRANCHIFULESS; r1 = NAT; COMMA; r2 = NAT; COMMA; target = IDENT; sep; instr = instructions { (BranchIfULess (r1, r2, target)) :: instr }
| PHI; d = NAT; COMMA; prev = IDENT; COMMA; r1 = NAT; COMMA; r2 = NAT; sep; instr = instructions { (Phi (d, prev, r1, r2)) :: instr }
| CALL; d = NAT; COMMA; scope = IDENT; COMMA; args = reg_list ; sep; instr = instructions { (Call (d, scope, args)) :: instr }
| RETURN; r = NAT; sep; instr = instructions { (Return r) :: instr }
| EXCL; c = COMMAND; sep; instr = instructions { (Command c) :: instr }

reg_list:
| sep { [] }
| r = NAT; sep { [ r ] }
| r = NAT; COMMA; l = reg_list { r :: l }

sep:
| LINE_END { }
| EOF { }