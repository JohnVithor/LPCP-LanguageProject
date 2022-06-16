module Parser where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Eval (eval)

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

plusToken = tokenPrim show updatePos get_token where
  get_token (Plus p) = Just (Plus p)
  get_token _      = Nothing

getDefaultValue (Type p "int") = Int p 0
getDefaultValue (Type p "real") = Real p 0.0
getDefaultValue (Type p "char") = Char p ' '
getDefaultValue (Type p "string") = String p ""
getDefaultValue (Type p "bool") = Bool p True
getDefaultValue _ = error "Error Is not a Type Token"

symtableInsert symbol []  = [symbol]
symtableInsert symbol symtable = symtable ++ [symbol]

-- concatenação de strings "string 1" + " string 2"
stringConcatExpression :: ParsecT [Token] [(Token,Token)] IO Token
stringConcatExpression = do
            a <- stringExpression
            b <- plusToken
            c <- stringExpression
            return (eval a b c)

-- Expressão envolvendo strings ou chars ou id
-- TODO: verificar o tipo de idToken
stringExpression = stringToken <|> charToken <|> idToken <|> stringConcatExpression

--parser para declaração de constante int
--constant int a = 10;
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

--int a = 10;
varInit :: ParsecT [Token] [(Token,Token)] IO [Token]
varInit = do
            a <- typeToken
            b <- idToken
            c <- assignToken
            d <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
            e <- semiColonToken
            -- TODO: validar o tipo (gramática de atributos)
            updateState(symtableInsert (b, getDefaultValue d))
            return (a:b:c:d:[e])

--int a;
varDeclaration :: ParsecT [Token] [(Token,Token)] IO [Token]
varDeclaration = do
            a <- typeToken
            b <- idToken
            c <- semiColonToken
            return (a:b:[c])

--a = 10;
varAssignment :: ParsecT [Token] [(Token,Token)] IO [Token]
varAssignment = do
            a <- idToken
            b <- assignToken
            c <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
            d <- semiColonToken
            updateState(symtableInsert (a, getDefaultValue c))
            return (a:b:c:[d])

updatePos pos _ (tok:_) = pos -- necessita melhoria
updatePos pos _ []      = pos

program :: ParsecT [Token] [(Token,Token)] IO [Token]
program = do
            a <- constantDecl
            eof
            return a

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program [] "Error message"
