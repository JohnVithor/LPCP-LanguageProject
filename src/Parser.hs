{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Eval (eval)
import TokenParser
import SymTable
import Control.Monad
import Type

structDeclaration :: ParsecT [Token] u IO Type
structDeclaration = do
            a <- beginScopeToken
            b <- structToken
            c <- idToken
            d <- colonToken
            e <- fieldDeclarations
            f <- endScopeToken
            g <- idToken
            h <- semiColonToken
            -- comparar c com g
            -- adicionar struct criada na tabela de tipos
            return (Type.Struct (getIdData c) e)

fieldDeclarations :: ParsecT [Token] u IO [(String, Type)]
fieldDeclarations = (do
                    a <- fieldDeclaration
                    b <- semiColonToken
                    return [a])
                    <|>
                    (do
                    c <- fieldDeclaration
                    d <- semiColonToken
                    e <- fieldDeclarations
                    return (c:e))

fieldDeclaration :: ParsecT [Token] u IO (String, Type)
fieldDeclaration = do
            a <- dataType
            b <- idToken
            c <- semiColonToken
            -- verificar se o id já não existe na tabela de simbolos
            return (getIdData b, a)

dataType :: ParsecT [Token] u IO Type
dataType = primitiveType <|> listType
        -- <|>
        -- (do 
        -- a <- idToken
        -- -- procurar id na tabela de tipos
        -- return tipo_tabela_tipos)

primitiveType :: ParsecT [Token] u IO Type.Type
primitiveType = intToken <|> realToken <|> boolToken <|> charToken <|> stringToken

listType :: ParsecT [Token] u IO Type
listType = simpleListType <|> doubleListType

simpleListType :: ParsecT [Token] u IO Type
simpleListType = do
            a <- primitiveType
            b <- beginListConstToken
            c <- endListConstToken
            return (Type.List [a])

doubleListType :: ParsecT [Token] u IO Type
doubleListType = do
            a <- simpleListType
            b <- beginListConstToken
            c <- endListConstToken
            return (Type.List [a])

-- concatenação de strings "string 1" + " string 2"
stringConcatExpression :: ParsecT [Token] [(Token,Token)] IO Type
stringConcatExpression = do
            a <- stringExpression
            b <- plusToken
            c <- stringExpression
            return (eval a b c)

-- Expressão envolvendo strings ou chars ou id
-- TODO: verificar o tipo de idToken
stringExpression :: ParsecT [Token] [(Token, Token)] IO Type
stringExpression = stringToken
               <|> charToken 
            --    <|> idToken
               <|> stringConcatExpression

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
            updateState(symtableInsert (b, d))
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
