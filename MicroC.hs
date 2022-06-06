-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO PRINCIPAL

module Main where

import Syntax
import TypeChecker
import Optimizer
import Generator
import Interpreter

import System.Environment
import System.Console.GetOpt

import Control.Monad.Except

data Flag = DumpOpt | DumpMach
 deriving (Show, Eq)

options = [ Option ['o']  ["dump-opt"]   (NoArg DumpOpt)
                   "Dumps optimized code"
          , Option ['m']  ["dump-mach"]  (NoArg DumpMach)
                   "Dumps machine code" ]
          
main = do args <- getArgs
          case  getOpt RequireOrder options args of
           (opts,args',[])   -> do prg <- readFile (args' !! 0)
                                   either putStr (process opts) (compile prg)
           (_,   _,    errs) -> ioError (userError (concat errs ++
                                                    usageInfo header options))
     where header = "Usage: MicroPascal [OPTIONS] file"  

compile prg = do  ast  <- parser prg
                  ast' <- case checkProgram ast of
                           []  -> return $ optimize ast
                           err -> throwError $ (unlines . map show) err 
                  return (ast', generate ast') 
                   
                  
process opts (ast,cod) = do when (elem DumpOpt  opts) (print ast)
                            when (elem DumpMach opts) (print cod)
                            interp [] cod ([],[])
                            return ()
