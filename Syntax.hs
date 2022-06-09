-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE SINTAXIS DEL LENGUAJE / PARSING


module Syntax where


import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Control.Monad

-- ABSTRACT SYNTAX TREE

newtype Program
    = Program MainBody
    deriving Show

type Name
    = String

data VarDef = VarDef Type Name 
              deriving Show

data Type = TyInt | TyChar  
            deriving Eq


instance Show Type where
    show TyInt  = "int"
    show TyChar = "char"


type MainBody = [CompoundStmt]

-- cuerpos de if y while
type Body = [Stmt]

-- cuerpos de programa principal
data CompoundStmt
    = Com Stmt
    | Decl VarDef
    deriving Show


data Stmt
    = StmtExpr  Expr
    | If      Expr Body Body
    | While   Expr Body
    | PutChar Expr
    deriving Show

data Expr
    = Var      Name
    | CharLit  Char
    | NatLit   Integer
    | GetChar
    | Unary    UOp Expr
    | Binary   BOp Expr Expr
    | Assign   Name Expr
    deriving (Eq,Show)

data UOp = Not | Neg
           deriving (Eq,Show)

data BOp
    = Or | And | Equ | Less 
    | Plus | Minus | Mult | Div | Mod
    deriving (Eq,Show)




-- PARSER

parser :: String -> Either String Program
parser = either (Left . show) Right . parse (programParser <* eof) ""

programParser
    = do m_whiteSpace
         body <- mainbodyparser
         option () (m_reserved ";")
         return (Program body)

varparser
    = do t <- typeparser
         v <- m_identifier
         return (VarDef t v) 

typeparser
    =   (m_reserved "int" >> return TyInt)
   <|>  (m_reserved "char" >> return TyChar)
                 

mainbodyparser :: Parser MainBody
mainbodyparser = compoundstmtparser `sepEndBy` m_semi
                     

bodyparser :: Parser Body
bodyparser
    =   m_braces (stmtparser `sepEndBy` m_semi)
   <|>  (:[]) <$> stmtparser              
   <|>  return []

compoundstmtparser :: Parser CompoundStmt
compoundstmtparser =   Com <$> stmtparser
                  <|>  (Decl <$> varparser)

stmtparser :: Parser Stmt
stmtparser 
    =  do  m_reserved "if"
           b <- m_parens exprparser
           ps <- bodyparser
           m_reserved "else"
           qs <- bodyparser
           return (If b ps qs)
   <|> do  m_reserved "while"
           b <- m_parens exprparser
           ps <- bodyparser
           return (While b ps)
   <|> do  m_reserved "putchar"
           e <- m_parens exprparser
           return (PutChar e)
   <|> StmtExpr <$> exprparser


exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table =
  [ [Prefix (m_reservedOp "~"  >> return (Unary Not))]
  , [Prefix (m_reservedOp "!"  >> return (Unary Not))]
  , [Prefix (m_reservedOp "-"  >> return (Unary Neg))]
  , [Infix (m_reservedOp  "&&" >> return (Binary And)) AssocLeft]
  , [Infix (m_reservedOp  "*"  >> return (Binary Mult)) AssocLeft]
  , [Infix (m_reservedOp  "/"  >> return (Binary Div)) AssocLeft]
  , [Infix (m_reservedOp  "%"  >> return (Binary Mod)) AssocLeft]
  , [Infix (m_reservedOp  "+"  >> return (Binary Plus)) AssocLeft]
  , [Infix (m_reservedOp  "-"  >> return (Binary Minus)) AssocLeft]
  , [Infix (m_reservedOp  "||" >> return (Binary Or)) AssocLeft]
  , [Infix (m_reservedOp  "==" >> return (Binary Equ)) AssocLeft]
  , [Infix (m_reservedOp  "<"  >> return (Binary Less)) AssocLeft]
  ]

term =  do m_reserved "getchar"
           m_parens spaces
           return (GetChar)
    <|> try (Assign <$> m_identifier <* m_reservedOp "=" <*> exprparser)
    <|> m_parens exprparser
    <|> Var <$> m_identifier 
    <|> (do m_whiteSpace
            char '\''
            c <- alphaNum
            char '\''
            m_whiteSpace
            return $ CharLit c)
    <|> fmap NatLit m_natural

def :: LanguageDef st
def = emptyDef{ commentStart = "(*"
              , commentEnd = "*)"
              , commentLine = "#"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "-<*+=%:aondm."
              , opLetter = oneOf "-<*+=%:andortivm."
              , reservedOpNames = [ "-", "<", "*", "+", "==", "="
                                  , "&&", "||", "~","/","%", ";", "!"]
              , reservedNames = ["true", "false"
                                , "int", "char"
                                , "if", "else"
                                , "while"
                                , "putchar", "getchar"]
              , caseSensitive = False
              }

TokenParser{ parens     = m_parens
           , brackets   = m_brackets
           , braces     = m_braces
           , semi       = m_semi
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved   = m_reserved
           , semiSep    = m_semiSep
           , semiSep1   = m_semiSep1
           , whiteSpace = m_whiteSpace
           , natural    = m_natural } = makeTokenParser def
