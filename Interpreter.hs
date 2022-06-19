-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DEL INTÉRPRETE DEL LENGUAJE DE MÁQUINA

module Interpreter where

import MachineLang
import Data.Char (ord, chr)

type Conf = (Stack,Env)

type Env = [(Var,Integer)]
type Stack = [Integer]

-- Implementar

-- interp :: Code -> Code -> Conf -> IO Conf
-- interp code [] conf                          = do print conf
--                                                   return conf

interp :: Code -> Code -> Conf -> IO Conf
interp code  []                conf          = return conf
-- PUSH
interp code' (i@(PUSH n):code) conf          = interp (i:code') code (push conf n)
-- ADD
interp code' (i@ADD:code) conf               = interp (i:code') code (bOp conf i)
-- SUB
interp code' (i@SUB:code) conf               = interp (i:code') code (bOp conf i)
-- MUL
interp code' (i@MUL:code) conf               = interp (i:code') code (bOp conf i)
-- DIV
interp code' (i@DIV:code) conf               = interp (i:code') code (bOp conf i)
-- MOD
interp code' (i@MOD:code) conf               = interp (i:code') code (bOp conf i)
-- CMP
interp code' (i@CMP:code) conf               = interp (i:code') code (bOp conf i)
-- JUMP
interp code' (i@(JUMP n):code) conf          = uncurry interp x conf
  where x = jump code' (i:code) $ toInteger n
-- JUMZ
interp code' (i@(JMPZ n):code) conf | v == 0 = uncurry interp c (pop conf)
                                    | v /= 0 = interp (i:code') code (pop conf)
  where (c,v) = ( jump code' (i:code) $ toInteger n
                , getValStack conf
                )
-- NEG
interp code' (i@NEG:code) conf               = interp (i:code') code (neg conf)
-- STORE
interp code' (i@(STORE name):code) conf      = interp (i:code') code (store conf name)
-- LOAD
interp code' (i@(LOAD name):code) conf       = interp (i:code') code (load conf name)
-- READ
interp code' (i@READ:code) conf              = do val <- getChar
                                                  interp (i:code') code (push conf $ charToInt val)
-- WRITE
interp code' (i@WRITE:code) conf             = do putChar c
                                                  interp (i:code') code (pop conf)
  where c = intToChar $ getValStack conf
-- SKIP
interp code' (i@SKIP:code) conf              = interp (i:code') code conf
-- Error
interp _ _ conf                              = error "interp: Error al interpretar el programa."

-- UTILS
-- -----

push :: Conf -> Integer -> Conf
push (stack, env) n = (n:stack, env)

pop :: Conf -> Conf
pop (n:stack, env) = (stack, env)
pop ([], _)        = error "pop: El stack está vacío."

neg :: Conf -> Conf
neg (x:stack, env) = (-x:stack, env)
neg ([], _)        = error "neg: El stack está vacío."

store :: Conf -> String -> Conf
-- store (val:stack, env) name = (stack, (name, val):env)
store (val:stack, env) name = if isNameEnv name env
                              then (stack, replaceVarEnv name val env)
                              else (stack, (name, val):env)
store _                _    = error "store: se rompió el stack :/"

load :: Conf -> String -> Conf
load (stack, env) name = (val:stack, env)
  where val = getValEnv env name

jump :: Code -> Code -> Integer -> (Code, Code)
jump c1     c2     0         = (c1, c2)
jump []     (y:c2) n | n > 0 = jump [y] c2 (n - 1)
jump (x:c1) []     n | n < 0 = jump c1 [x] (n + 1)
jump (x:c1) (y:c2) n | n > 0 = jump (y:x:c1) c2 (n - 1)
                     | n < 0 = jump c1 (x:y:c2) (n + 1)
jump _ _ _                   = error "jump: La dirección de salto es incorrecta."

cmp :: Integer -> Integer -> Integer
cmp x y | x == y    = 0
        | x > y     = 1
        | otherwise = -1

isNameEnv :: Var -> Env -> Bool
isNameEnv name env = elem name $ map fst env

replaceVarEnv :: Var -> Integer -> Env -> Env
replaceVarEnv name v (e@(n, _):env) = if name == n
                                    then (name, v):replaceVarEnv name v env
                                    else e:replaceVarEnv name v env
replaceVarEnv name v [] = []

getValStack :: Conf -> Integer
getValStack (v:stack, env) = v
getValStack ([], _)        = error "getValStack: El stack está vacío."

bOp :: Conf -> Instr -> Conf
bOp e@(stack, env) instr = case instr of
                             ADD -> push env' (x + y)
                             SUB -> push env' (x - y)
                             MUL -> push env' (x * y)
                             DIV -> push env' (x `div` y)
                             MOD -> push env' (x `mod` y)
                             CMP -> push env' (cmp x y)
                             _   -> error "bOp: El operador no es binario."
  where (x, y, env') = ( getValStack e
                       , (getValStack . pop) e
                       , (pop . pop) e -- Pop del stack de los operadores
                       )



getValEnv :: Env -> Var -> Integer
getValEnv [] name = error "getValEnv: No se encontró la variable en el ambiente."
getValEnv (x:xs) name = if name == fst x
                        then snd x
                        else getValEnv xs name


getValPush :: Instr -> Integer
getValPush (PUSH n) = n
getValPush _        = error "getValPush: La instrucción no es PUSH."

charToInt :: Char -> Integer
charToInt = toInteger . ord

intToChar :: Integer -> Char
intToChar = chr . fromIntegral
