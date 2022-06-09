-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE OPTIMIZACIÓN

module Optimizer where

import Syntax


-- Implementar
optimize :: Program -> Program
optimize (Program body) = Program ((deadCodeElim . foldProg) body)

-- CONSTANT FOLDING
-- ----------------

foldProg :: MainBody -> MainBody
foldProg []                      = []
foldProg (d@(Decl _):statements) = d:foldProg statements
foldProg ((Com stmt):statements) = Com (foldStmt stmt):foldProg statements

foldBody :: Body -> Body
foldBody = map foldStmt

foldStmt :: Stmt -> Stmt
foldStmt (StmtExpr e) = StmtExpr (foldExpr e)
foldStmt (If e b1 b2) = If (foldExpr e) (foldBody b1) (foldBody b2)
foldStmt (While e b)  = While (foldExpr e) (foldBody b)
foldStmt (PutChar e)  = PutChar (foldExpr e)

foldExpr :: Expr -> Expr
foldExpr v@(Var name)       = v
foldExpr c@(CharLit char)   = c
foldExpr n@(NatLit integer) = n
foldExpr GetChar            = GetChar
foldExpr (Unary uOp e)      = Unary uOp (foldExpr e)
foldExpr (Binary bOp e1 e2) = constFold e1' e2' bOp
  where (e1',e2') = (foldExpr e1, foldExpr e2)
foldExpr (Assign name e)    = Assign name (foldExpr e)


constFold :: Expr -> Expr -> BOp -> Expr
constFold e1 e2 bOp | isNeutro e1 bOp                          = e2
                    | isNeutro e2 bOp                          = e1
                    | isNulo e1 bOp                            = e1
                    | isNulo e2 bOp                            = e2
                    | isNat e1 && isNat e2 && isAritmetico bOp = NatLit (operacion e1 e2 bOp)
                    | otherwise                                = Binary bOp e1 e2

-- DEAD CODE ELIMINATION
-- ---------------------

deadCodeElim :: MainBody -> MainBody
deadCodeElim []                      = []
deadCodeElim (d@(Decl _):statements) = d:deadCodeElim statements
-- Dead code elimination de statements del MainBody
deadCodeElim ((Com stmt):statements) = codeElimStmt stmt ++ deadCodeElim statements

codeElimStmt :: Stmt -> [CompoundStmt]
codeElimStmt stmt@(StmtExpr e)                          = [Com stmt]
codeElimStmt put@(PutChar e)                            = [Com put]
-- Dead code elimination If
codeElimStmt (If e b1 b2) | isNat e && isTrue e         = map Com (codeElimBody b1)
                          | isNat e && (not . isTrue) e = map Com (codeElimBody b2)
                          | otherwise                   = [Com (If e (codeElimBody b1) (codeElimBody b2))]
-- Dead code elimination While
codeElimStmt (While e b) | isNat e && (not . isTrue) e  = []
                         | otherwise                    = [Com (While e (codeElimBody b))]

codeElimBody :: Body -> Body
codeElimBody = concatMap codeElimBodyStmt

codeElimBodyStmt :: Stmt -> [Stmt]
codeElimBodyStmt stmt@(StmtExpr e)                          = [stmt]
codeElimBodyStmt put@(PutChar e)                            = [put]
codeElimBodyStmt (If e b1 b2) | isNat e && isTrue e         = codeElimBody b1
                              | isNat e && (not . isTrue) e = codeElimBody b2
                              | otherwise                   = [If e (codeElimBody b1) (codeElimBody b2)]
codeElimBodyStmt (While e b) | isNat e && (not . isTrue) e  = []
                             | otherwise                    = [While e (codeElimBody b)]

-- UTILS
-- -----

isTrue :: Expr -> Bool
isTrue (NatLit n) = n > 0
isTrue _          = error "isTrue: La expresión no es de tipo NatLit."

isNeutro :: Expr -> BOp -> Bool
isNeutro (NatLit int) Plus = int == 0
isNeutro (NatLit int) Mult = int == 1
isNeutro (NatLit int) And  = int /= 0
isNeutro (NatLit int) Or   = int == 0
isNeutro _            _    = False

isNulo :: Expr -> BOp -> Bool
isNulo (NatLit int) Mult = int == 0
isNulo (NatLit int) And  = int == 0
isNulo (NatLit int) Or   = int /= 0
isNulo _            _    = False

isNat :: Expr -> Bool
isNat (NatLit n) = True
isNat _          = False

getNat :: Expr -> Integer
getNat (NatLit n) = n
getNat _          = error "getNat: La expresión no es de tipo NatLit."

operacion :: Expr -> Expr -> BOp -> Integer
operacion e1 e2 Plus  = getNat e1 + getNat e2
operacion e1 e2 Minus = getNat e1 - getNat e2
operacion e1 e2 Mult  = getNat e1 * getNat e2
operacion e1 e2 Div   = getNat e1 `div` getNat e2
operacion e1 e2 Mod   = getNat e1 `mod` getNat e2
operacion _  _  _     = error "opreacion: Operador aritmético incorrecto."

isAritmetico :: BOp -> Bool
isAritmetico op = case op of
                    Plus  -> True
                    Minus -> True
                    Mult  -> True
                    Div   -> True
                    Mod   -> True
                    _     -> False

getMainBody :: Program -> MainBody
getMainBody (Program body) = body

-- Función auxiliar para usar en el REPL
getProgram :: Either a b -> b
getProgram (Right p) = p
getProgram _         = error "getProgram: Error de sintáxis."
