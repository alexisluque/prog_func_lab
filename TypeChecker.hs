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
checkProgram (Program body) | null errors = checkProgType body []
                            | otherwise   = errors
   where errors = checkProg body []

-- CHECKEO DE NOMBRES
-- ------------------

-- MainBody ([CompoundStmt (Com | Decl)])
checkProg :: MainBody -> Env -> [Error]
checkProg []                 _   = []
-- Checkeo de varibles indefinindas
checkProg ((Com stmt):xs)    env = checkStmt stmt env ++ checkProg xs env
-- Checkeo de variables duplicadas
checkProg ((Decl varDef):xs) env = checkVarDef varDef env ++ checkProg xs (varDefToEnv varDef:env)

--  Checkeo de variables duplicados
-- VarDef Type Nane
checkVarDef :: VarDef -> Env -> [Error]
checkVarDef (VarDef ty name) env = [Duplicated name | isInEnv name env]

-- Checkeo de variables indefinidas
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
checkExpr (Var name)         env                    = [Undefined name | not $ isInEnv name env]
checkExpr (CharLit _)        _                      = []
checkExpr (NatLit _)         _                      = []
checkExpr GetChar            _                      = []
checkExpr (Unary _ expr)     env                    = checkExpr expr env
checkExpr (Binary _ e1 e2)   env                    = checkExpr e1 env ++ checkExpr e2 env
checkExpr (Assign name expr) env | isInEnv name env = checkExpr expr env
                                 | otherwise        = Undefined name:checkExpr expr env

-- CHECKEO DE TIPOS
-- ----------------

checkProgType :: MainBody -> Env -> [Error]
checkProgType []              _   = []
-- Checkeo de tipos
checkProgType ((Com stmt):xs) env = checkStmtType stmt env TyChar ++ checkProgType xs env
-- Agregar variables a Env
checkProgType ((Decl varDef):xs)   env = checkProgType xs (varDefToEnv varDef:env)

checkBodyType :: Body -> Env -> Type -> [Error]
checkBodyType [] env _ = []
checkBodyType (stmt:statements) env _ = checkStmtType stmt env TyChar ++ checkBodyType statements env TyChar

checkStmtType :: Stmt -> Env -> Type -> [Error]
checkStmtType (StmtExpr e) env _ = checkExprType e env (getExprType e env)
checkStmtType (If e b1 b2) env _ = checkExprType e env TyInt
checkStmtType (While e b)  env _ = checkExprType e env TyInt
checkStmtType (PutChar e)  env _ = checkExprType e env TyChar

checkExprType :: Expr -> Env -> Type -> [Error]
checkExprType (Var name)          env ty = [Expected ty (getVarType name env) | ty /= getVarType name env]
checkExprType (CharLit char)      _   ty = [Expected ty TyChar | ty /= TyChar]
checkExprType (NatLit int)        _   ty = [Expected ty TyInt | ty /= TyInt]
checkExprType GetChar             _   ty = [Expected ty TyChar | ty /= TyChar]
checkExprType (Unary _ expr)      env ty = [Expected ty TyInt | ty /= TyInt] ++ checkExprType expr env TyInt
checkExprType (Binary Equ e1 e2)  env ty = [Expected ty ty' | ty /= ty'] ++ checkExprType e1 env ty' ++ checkExprType e2 env ty'
   where ty' = getExprType e1 env
checkExprType (Binary Less e1 e2) env ty = [Expected ty ty' | ty /= ty'] ++ checkExprType e1 env ty' ++ checkExprType e2 env ty'
   where ty' = getExprType e1 env
checkExprType (Binary _ e1 e2)    env ty = [Expected ty TyInt | ty /= TyInt] ++ checkExprType e1 env TyInt ++ checkExprType e2 env TyInt
checkExprType (Assign name expr)  env ty = [Expected ty ty' | ty /= ty'] ++ checkExprType expr env ty'
   where ty' = getVarType name env

getExprType :: Expr -> Env -> Type
getExprType (Var name)         env = getVarType name env
getExprType (CharLit char)     _   = TyChar
getExprType (NatLit int)       _   = TyInt
getExprType GetChar            env = TyChar
getExprType (Unary _ expr)     env = getExprType expr env
getExprType (Binary _ e1 e2)   env = getExprType e1 env
getExprType (Assign name expr) env = getVarType name env

-- UTILS
-- -----

-- Transforma (VarDef type name) a (name, type)
varDefToEnv :: VarDef -> (Name, Type)
varDefToEnv (VarDef ty name) = (name, ty)

-- Checkea si la variable esté en el ambiente
isInEnv :: Name -> Env -> Bool
isInEnv name env = elem name $ map fst env

getVarType :: Name -> Env -> Type
getVarType name env = snd $ head $ filter (\(n,ty) -> n == name) env

-- Función auxiliar para usar en el REPL
getProgram :: Either a b -> b
getProgram (Right p) = p
