{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Eval (eval)
import TokenParser
import SymTable

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
constantDecl :: ParsecT [Token] [(Token, Token)] IO [Token]
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

program :: ParsecT [Token] [(Token,Token)] IO [Token]
program = do
            a <- constantDecl
            eof
            return a

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program [] "Error message"
