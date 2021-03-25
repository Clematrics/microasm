type scope = int

type register = int

type block = int

type instr =
  | Add of register * register * register
  | Sub of register * register * register
  | Mul of register * register * register
  | Div of register * register * register
  | Rem of register * register * register
  | Not of register * register
  | And of register * register * register
  | Or of register * register * register
  | Xor of register * register * register
  | BranchIfZero of register * block
  | BranchIfLess of register * register
  | Phi of register * (block * register) * (block * register)
  | Call of scope * register list
