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
genMainBody []                         = [SKIP]
genMainBody ((Decl varDef):statements) = genMainBody statements
genMainBody ((Com stmt):statements)    = genStmt stmt ++ genMainBody statements

genVarDef :: VarDef -> Instr
genVarDef (VarDef _ name) = SKIP

genStmt :: Stmt -> Code
genStmt (StmtExpr e) = genExpr e
genStmt (If e b1 b2) = genExpr e
                       ++ [ PUSH 0
                          , CMP
                          , JMPZ (length b1' + 2) -- JMPZ ELSE
                          ]
                       ++ b1'
                       ++ [JUMP (length b2')] -- JUMP END
                       -- ELSE:
                       ++ b2'
                      --  END
  where (b1', b2') = (genBody b1, genBody b2)
genStmt (While e b)  = e'
                       ++ [ -- COND:
                          PUSH 0
                          , CMP
                          , JMPZ (length b' + 2) -- JMPZ END
                          ]
                       ++ b'
                       ++ [JMPZ (-length b' - length e' - 3)] -- JUMP COND
                      -- END:
  where (e',b') = (genBody b, genExpr e)
genStmt (PutChar e)  = [PUSH (charLitToInt e), WRITE]

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
genExpr (Binary Equ e1 e2)   = e2' ++ e1' ++ [CMP]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- LESS
genExpr (Binary Less e1 e2)  = e2' ++ e1'
                               ++ [ CMP
                                  , PUSH 1
                                  , ADD
                                  , JMPZ 4
                                  , STORE "_pop"
                                  , PUSH 0
                                  , JUMP 2
                                  , PUSH 1
                                  , SKIP
                                  ]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- AND
genExpr (Binary And e1 e2)   = e2' ++ e1'
                               ++ [ PUSH 0
                                  , CMP
                                  , JMPZ 8       --JUMP X_TRUE. if (x != 0)
                                  , STORE "_pop" -- Sacar resutado CMP 0 x del stack
                                  , PUSH 0
                                  , CMP
                                  , JMPZ 7       -- JUMP END. if (y != 0)
                                  , STORE "_pop" -- Sacar resutado CMP 0 y del stack
                                  , PUSH 1
                                  , JUMP 4       -- JUMP END end if
                                  -- X_TRUE:
                                  , STORE "_pop" -- else (x != 0) Sacar 0 del stack (CMP 0 x)
                                  , STORE "_pop" -- Sacar y del stack
                                  , PUSH 0
                                  -- END:
                                  , SKIP         -- else (y != 0)
                                  ]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- OR
genExpr (Binary Or e1 e2)    = e2' ++ e1'
                               ++ [ PUSH 1
                                  , CMP
                                  , JMPZ 8       -- JMPZ X_FALSE. if (x != 1)
                                  , STORE "_pop" -- Sacar resutado CMP 1 x del stack
                                  , PUSH 1
                                  , CMP
                                  , JMPZ 4       -- JUMP X_FALSE. if (y != 1)
                                  , STORE "_pop" -- Sacar resutado CMP 1 y del stack
                                  , PUSH 0
                                  , JUMP 4       -- JUMP END. end if
                                    -- X_FALSE:
                                  , STORE "_pop" -- else. Sacar 0 del stack (CMP 1 x)
                                  , STORE "_pop" -- Sacar y del stack
                                  , PUSH 1
                                  -- END:
                                  , SKIP
                                  ]
  where (e1',e2') = (genExpr e1, genExpr e2)
-- ASSIGN
genExpr (Assign name e)      = e' ++ [STORE name]
  where e' = genExpr e

-- UTILS
-- -----

aritmetic :: Expr -> Expr -> Instr -> Code
aritmetic e1 e2 instr = [getInstr e1, getInstr e2, instr]

getInstr :: Expr -> Instr
getInstr e | isConst e = PUSH (getConst e)
           | otherwise = LOAD (getVarName e)

getConst :: Expr -> Integer
getConst e = case e of
              (NatLit n)  -> n
              (CharLit c) -> toInteger $ ord c
              _           -> error "getConst: La expresión no es NatLit o charLit."

isConst :: Expr -> Bool
isConst e = case e of
              (NatLit _)  -> True
              (CharLit _) -> True
              _           -> False

getVarName :: Expr -> Name
getVarName (Var name) = name
getVarName _          = error "getVarName: La expresión no es Var."

notValue :: Expr -> Integer
notValue (NatLit n)  = not' n
notValue (CharLit c) = (not' . toInteger . ord) c
notValue _           = error "notValue: La expresión no es NatLit o charLit."

not' :: Integer -> Integer
not' n | n == 0    = 1
       | otherwise = 0

charLitToInt :: Expr -> Integer
charLitToInt (CharLit c) = toInteger $ ord c
charLitToInt _           = error "getCharLit: La expresión no es un carácter."

-- Función auxiliar para usar en el REPL
getProgram :: Either a b -> b
getProgram (Right p) = p
getProgram _         = error "getProgram: Error de sintáxis."
