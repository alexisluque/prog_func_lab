-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE SINTAXIS DEL LENGUAJE DE MÁQUINA


module MachineLang where

type Code = [Instr]

data Instr = NEG
           | ADD
           | SUB
           | MUL
           | DIV
           | MOD
           | CMP
           | PUSH  Integer
           | JUMP  Shift
           | JMPZ  Shift
           | LOAD  Var
           | STORE Var
           | READ
           | WRITE
           | SKIP
 deriving Show

type Shift = Int
type Var   = String

