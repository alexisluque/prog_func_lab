-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE GENERACIÓN DE CÓDIGO DE MÁQUINA

module Generator where

import Syntax
import MachineLang
import Data.Char

-- Implementar
generate :: Program -> Code
generate (Program body) = genMainBody body

genMainBody :: MainBody -> Code
genMainBody []                      = [SKIP]
genMainBody ((Decl _):statements)   = genMainBody statements
genMainBody ((Com stmt):statements) = assign stmt ++ genMainBody statements

assign :: Stmt -> Code
assign (StmtExpr (Assign name e)) = genExpr e ++ [STORE name]
assign stmt                       = genStmt stmt

genStmt :: Stmt -> Code
genStmt (StmtExpr e) = genExpr e
genStmt (If e b1 b2) = genExpr e
                       ++ [ PUSH 0
                          , CMP
                          , JMPZ (length b1' + 2) -- JMPZ ELSE
                          ]
                       ++ b1'
                       ++ [ JUMP (length b2' + 1)] -- JUMP END
                       -- ELSE:
                       ++ b2'
                       --  END
  where (b1', b2') = (genBody b1, genBody b2)
genStmt (While e b)  = e'
                       ++ [ PUSH 0 -- COND:
                          , CMP
                          , JMPZ (length b' + 2) -- JMPZ END
                          ]
                       ++ b'
                       ++ [JUMP (-(length e' + length b' + 3))] -- JUMP COND
                       -- END:
  where (e',b') = (genExpr e, genBody b)
genStmt (PutChar e)  = if isCharLit e
                       then [PUSH val, WRITE]
                       else [LOAD name, WRITE]
  where (val, name) = (charLitToInt e, getVarName e)

genBody :: Body -> Code
genBody = concatMap genStmt

genExpr :: Expr -> Code
genExpr (Var name)           = [LOAD name]
genExpr (CharLit char)       = [PUSH (toInteger $ ord char)]
genExpr (NatLit n)           = [PUSH n]
genExpr GetChar              = [READ]
-- NEG
genExpr (Unary Neg e)        = genExpr e ++ [NEG]
-- NOT
genExpr (Unary Not e)        = e'
                               ++ [ PUSH 0
                                  , CMP
                                  , JMPZ 3 -- JMPZ TRUE
                                  , PUSH 0
                                  , JUMP 2 -- JUMP END
                                  -- TRUE:
                                  , PUSH 1
                                  -- END:
                                  ]
  where e' = genExpr e
-- PLUS
genExpr (Binary Plus e1 e2)  = e2' ++ e1' ++ [ADD]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- MINUS
genExpr (Binary Minus e1 e2) = e2' ++ e1' ++ [SUB]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- MULT
genExpr (Binary Mult e1 e2)  = e2' ++ e1' ++ [MUL]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- DIV
genExpr (Binary Div e1 e2)   = e2' ++ e1' ++ [DIV]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- MOD
genExpr (Binary Mod e1 e2)   = e2' ++ e1' ++ [MOD]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- EQU
genExpr (Binary Equ e1 e2)   = e2' ++ e1'
                               ++ [ CMP
                                  , JMPZ 3
                                  , PUSH 0
                                  , JUMP 2
                                  , PUSH 1
                                  ]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- LESS
genExpr (Binary Less e1 e2)  = e2' ++ e1'
                               ++ [ CMP
                                  , PUSH 1
                                  , ADD
                                  , JMPZ 3 -- JMPZ TRUE
                                  , PUSH 0
                                  , JUMP 2 -- JUMP END
                                  -- TRUE:
                                  , PUSH 1
                                  -- END:
                                  ]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- AND
genExpr (Binary And e1 e2)   = e2' ++ e1'
                               ++ [ PUSH 0
                                  , CMP
                                  , JMPZ 7 --JUMP TRUE
                                  , PUSH 0
                                  , CMP
                                  , JMPZ 4 -- JUMP END
                                  , PUSH 1
                                  , JUMP 3 -- JUMP END
                                  , JMPZ 1 -- POP y
                                  -- TRUE:
                                  , PUSH 0
                                  -- END:
                                  ]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- OR
genExpr (Binary Or e1 e2)    = e2' ++ e1'
                               ++ [ PUSH 1
                                  , CMP
                                  , JMPZ 6 --JUMP TRUE
                                  , PUSH 1
                                  , CMP
                                  , JMPZ 4 -- JUMP END
                                  , PUSH 0
                                  , JUMP 3 -- JUMP END
                                  , JMPZ 1 -- POP y
                                  -- TRUE:
                                  , PUSH 1
                                  -- END:
                                  ]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- ASSIGN
genExpr (Assign name e)      = e' ++ [STORE name, LOAD name]
  where e' = genExpr e

-- UTILS
-- -----

charLitToInt :: Expr -> Integer
charLitToInt (CharLit c) = toInteger $ ord c
charLitToInt _           = error "charLitToInt: La expresión no es de tipo CharLit."

isCharLit :: Expr -> Bool
isCharLit (CharLit c) = True
isCharLit _           = False

getVarName :: Expr -> String
getVarName (Var name) = name
getVarName _          = error "getVarName: La expresión no es de tipo Var."

isAssign :: Stmt -> Bool
isAssign (StmtExpr (Assign _ _)) = True
isAssign _                       = False

-- Función auxiliar para usar en el REPL
getProgram :: Either a b -> b
getProgram (Right p) = p
getProgram _         = error "getProgram: Error de sintáxis."
