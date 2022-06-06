-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DEL INTÉRPRETE DEL LENGUAJE DE MÁQUINA

module Interpreter where

import MachineLang

type Conf = (Stack,Env)

type Env = [(Var,Integer)]
type Stack = [Integer]

-- Implementar
interp :: Code -> Code -> Conf -> IO Conf
interp = undefined
