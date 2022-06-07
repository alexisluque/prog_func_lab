-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE CHEQUEO DE NOMBRES Y TIPOS

module TypeChecker where

import Syntax


data Error = Duplicated      Name
           | Undefined       Name
           | Expected        Type Type

instance Show Error where
   show (Duplicated      n)  = "Duplicated definition: " ++ n
   show (Undefined       n)  = "Undefined: " ++ n
   show (Expected    ty ty') = "Expected: " ++ show ty
                             ++ " Actual: " ++ show ty'

type Env = [(Name, Type)]

-- Implementar
checkProgram :: Program -> [Error]
checkProgram (Program mainBody) = checkProg mainBody []

-- MainBody ([CompoundStmt (Com | Decl)])
checkProg :: MainBody -> Env -> [Error]
checkProg []                 env = []
checkProg ((Com stmt):xs)    env = checkStmt stmt env
checkProg ((Decl varDef):xs) env = checkVarDef varDef env ++ checkProg xs (varDefToEnv varDef:env)

-- VarDef Type Nane
checkVarDef :: VarDef -> Env -> [Error]
checkVarDef (VarDef ty name) env | isInEnv name env = [Duplicated name]
                                 | otherwise        = []

-- Body ([Stmt])
checkBody :: Body -> Env -> [Error]
checkBody [] _ = []
checkBody (x:xs) env = checkStmt x env ++ checkBody xs env

-- Stmt (Expr | If | While | PutChar)
checkStmt :: Stmt -> Env -> [Error]
checkStmt (StmtExpr expr) env = checkExpr expr env
checkStmt (If expr b1 b2) env = checkExpr expr env ++ checkBody b1 env ++ checkBody b2 env
checkStmt (While expr b)  env = checkExpr expr env ++ checkBody b env
checkStmt (PutChar expr)  env = checkExpr expr env

-- Expr (Var | CahrLit| NatLit | GetChar | Unary | Binary | Assign)
checkExpr :: Expr -> Env -> [Error]
checkExpr (Var name)         env | isInEnv name env = []
                                 | otherwise        = [Undefined name]
checkExpr (CharLit _)        _                      = []
checkExpr (NatLit _)         _                      = []
checkExpr GetChar env                               = []
checkExpr (Unary _ expr)     env                    = checkExpr expr env
checkExpr (Binary _ e1 e2)   env                    = checkExpr e1 env ++ checkExpr e2 env
checkExpr (Assign name expr) env | isInEnv name env = checkExpr expr env
                                 | otherwise        = Undefined name:checkExpr expr env

-- Transforma (VarDef type name) a (name, type)
varDefToEnv :: VarDef -> (Name, Type)
varDefToEnv (VarDef ty name) = (name, ty)

-- Checkea si la variable esté en el ambiente
isInEnv :: Name -> Env -> Bool
isInEnv name env = elem name $ map fst env

-- Función auxiliar para usar en el REPL
getProgram :: Either a b -> b
getProgram (Right p) = p
