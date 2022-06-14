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
checkProgram (Program body) | null nameErr = checkProgType body []
                            | otherwise    = nameErr
   where nameErr = checkProg body []

-- CHECKEO DE NOMBRES
-- ------------------

-- MainBody ([CompoundStmt (Com | Decl)])
checkProg :: MainBody -> Env -> [Error]
checkProg []                 _   = []
-- Checkeo de varibles indefinindas
checkProg ((Com stmt):xs)    env = checkStmt stmt env ++ checkProg xs env
-- Checkeo de variables duplicadas
checkProg ((Decl varDef):xs) env = checkVarDef varDef env
                                   ++ checkProg xs (varDefToEnv varDef:env)

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
checkStmt (If expr b1 b2) env = checkExpr expr env ++ checkBody b1 env
                                ++ checkBody b2 env
checkStmt (While expr b)  env = checkExpr expr env ++ checkBody b env
checkStmt (PutChar expr)  env = checkExpr expr env

-- Expr (Var | CahrLit| NatLit | GetChar | Unary | Binary | Assign)
checkExpr :: Expr -> Env -> [Error]
checkExpr (Var name)         env = [Undefined name | not $ isInEnv name env]
checkExpr (CharLit _)        _   = []
checkExpr (NatLit _)         _   = []
checkExpr GetChar            _   = []
checkExpr (Unary _ expr)     env = checkExpr expr env
checkExpr (Binary _ e1 e2)   env = checkExpr e1 env ++ checkExpr e2 env
checkExpr (Assign name expr) env = if isInEnv name env then
                                     checkExpr expr env
                                   else
                                      Undefined name:checkExpr expr env

-- CHECKEO DE TIPOS
-- ----------------

checkProgType :: MainBody -> Env -> [Error]
checkProgType []                 _   = []
-- Checkeo de tipos
checkProgType ((Com stmt):xs)    env = checkStmtType stmt env
                                       ++ checkProgType xs env
-- Agregar variables a Env
checkProgType ((Decl varDef):xs) env = checkProgType xs (varDefToEnv varDef:env)

checkBodyType :: Body -> Env -> [Error]
checkBodyType statements env
  = foldr
      (\ stmt -> (++) (checkStmtType stmt env)) [] statements

checkStmtType :: Stmt -> Env -> [Error]
checkStmtType (StmtExpr e) env = checkExprType e env
checkStmtType (If e b1 b2) env = getError TyInt ty
                                 ++ checkExprType e env
                                 ++ checkBodyType b1 env
                                 ++ checkBodyType b2 env
  where ty = getExprType e env
checkStmtType (While e b)  env = getError TyInt ty
                                 ++ checkExprType e env
                                 ++ checkBodyType b env
  where ty = getExprType e env
checkStmtType (PutChar e)  env = checkExprType e env
                                 ++ getError TyChar ty
  where ty = getExprType e env

checkExprType :: Expr -> Env -> [Error]
checkExprType (Var name)          _ = []
checkExprType (CharLit char)      _ = []
checkExprType (NatLit int)        _ = []
checkExprType GetChar             _ = []
checkExprType (Unary _ expr)    env = checkExprType expr env
checkExprType (Binary op e1 e2) env = case op of
                                        Equ  -> e1' ++ e2'
                                                ++ getError ty1 ty2
                                        Less -> e1' ++ e2'
                                                ++ getError ty1 ty2
                                        _    -> e1' ++ e2'
                                                ++ getError TyInt ty1
                                                ++ getError TyInt ty2
  where (ty1, ty2, e1', e2') = ( getExprType e1 env
                               , getExprType e2 env
                               , checkExprType e1 env
                               , checkExprType e2 env
                               )
checkExprType (Assign name expr) env = e ++ getError tyVar tyExpr
  where (tyVar, tyExpr, e) = ( getVarType name env
                             , getExprType expr env
                             , checkExprType expr env
                             )

-- UTILS
-- -----

getError :: Type -> Type -> [Error]
getError expected actual = [Expected expected actual | expected /= actual]

getExprType :: Expr -> Env -> Type
getExprType (Var name)         env = getVarType name env
getExprType (CharLit char)     _   = TyChar
getExprType (NatLit int)       _   = TyInt
getExprType GetChar            _   = TyChar
getExprType (Unary _ expr)     env = getExprType expr env
getExprType (Binary _ e1 e2)   _   = TyInt
getExprType (Assign name expr) env = getVarType name env

-- Transforma (VarDef type name) a (name, type)
varDefToEnv :: VarDef -> (Name, Type)
varDefToEnv (VarDef ty name) = (name, ty)

-- Checkea si la variable esté en el ambiente
isInEnv :: Name -> Env -> Bool
isInEnv name env = elem name $ map fst env

getVarType :: Name -> Env -> Type
getVarType name env = snd $ head $ filter (\(n,_) -> n == name) env

-- Función auxiliar para usar en el REPL
getProgram :: Either a b -> b
getProgram (Right p) = p
getProgram _         = error "getProgram: Error de sintáxis."
