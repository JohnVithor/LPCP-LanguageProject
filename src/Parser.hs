module Parser where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Control.Exception (assert)
import GHC.IO.Exception (assertError)

-- parsers para os terminais TODO:

globalToken :: ParsecT [Token] u IO Token
globalToken = tokenPrim show updatePos get_token where
  get_token (Global p) = Just (Global p)
  get_token _          = Nothing


intToken = tokenPrim show updatePos get_token where
  get_token (Int p x) = Just (Int p x)
  get_token _       = Nothing

stringToken = tokenPrim show updatePos get_token where
  get_token (String p x) = Just (String p x)
  get_token _       = Nothing

realToken = tokenPrim show updatePos get_token where
  get_token (Real p x) = Just (Real p x)
  get_token _       = Nothing

charToken = tokenPrim show updatePos get_token where
  get_token (Char p x) = Just (Char p x)
  get_token _       = Nothing

boolToken = tokenPrim show updatePos get_token where
  get_token (Bool p x) = Just (Bool p x)
  get_token _       = Nothing

constantToken = tokenPrim show updatePos get_token where
  get_token (Constant p) = Just (Constant p)
  get_token _        = Nothing 

semiColonToken = tokenPrim show updatePos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _         = Nothing

assignToken = tokenPrim show updatePos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _      = Nothing

idToken = tokenPrim show updatePos get_token where
  get_token (Id p x) = Just (Id p x)
  get_token _      = Nothing

typeToken = tokenPrim show updatePos get_token where
  get_token (Type p x) = Just (Type p x)
  get_token _        = Nothing 

getDefaultValue (Type p "int") = Int p 0          

symtableInsert symbol []  = [symbol]
symtableInsert symbol symtable = symtable ++ [symbol]

--parser para declaração de constante int
--constant int a = 10;
constantDecl :: ParsecT [Token] [(Token,Token)] IO [Token]
constantDecl = do
            const <- constantToken
            a <- typeToken
            b <- idToken
            c <- assignToken
            d <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
            e <- semiColonToken
            -- TODO: validar o tipo (gramática de atributos)
            updateState(symtableInsert (b, getDefaultValue d))
            return (const:a:b:c:d:[e])


updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (tok:_) = pos -- necessita melhoria
updatePos pos _ []      = pos

program :: ParsecT [Token] [(Token,Token)] IO [Token]
program = do
            a <- constantDecl
            eof
            return a

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program [] "Error message"

