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
import Text.Parsec.Token (GenTokenParser(semi))
import Data.Maybe

structDeclarations :: ParsecT [Token] ([(String, Type)], [Type]) IO [Type]
structDeclarations = (do
                    c <- structDeclaration
                    e <- structDeclarations
                    return (c:e))
                    <|> return []

structDeclaration :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
structDeclaration = do
            a <- beginScopeToken
            b <- structToken
            c <- idToken
            d <- colonToken
            e <- fieldCreations
            f <- endScopeToken
            g <- idToken
            h <- semiColonToken
            -- comparar c com g
            -- adicionar struct criada na tabela de tipos
            updateState(typeTableInsert (Type.Struct (getIdData c) e))
            return (Type.Bool False)

fieldCreations :: ParsecT [Token] ([(String,Type)], [Type]) IO [(String, Type)]
fieldCreations = (do
                    c <- fieldCreation
                    e <- fieldCreations
                    return (c:e))
                    <|> return []

fieldCreation :: ParsecT [Token] ([(String, Type)], [Type]) IO (String, Type)
fieldCreation = do
            a <- typeToken <|> idToken
            s <- getState
            b <- idToken
            c <- initialization (typeTableGet a s)
            return (getIdData b, c)


initialization :: Type -> ParsecT [Token] ([(String, Type)], [Type]) IO Type
initialization t = try (do
        c <- assignToken
        d <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
        e <- semiColonToken
        if compatible t d then return d else fail "tipos diferentes")
        <|> 
        (do 
        e <- semiColonToken
        return t)

compatible :: Type -> Type -> Bool
compatible (Type.Int _) (Type.Int _) = True
compatible (Type.Real _) (Type.Real _) = True
compatible (Type.String _) (Type.String _) = True
compatible (Type.Char _) (Type.Char _) = True
compatible (Type.Bool _) (Type.Bool _) = True
compatible _ _ = False

dataType :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
dataType = try primitiveType <|> listType
        -- <|>
        -- (do 
        -- a <- idToken
        -- -- procurar id na tabela de tipos
        -- return tipo_tabela_tipos)

primitiveType :: ParsecT [Token] ([(String,Type)], [Type]) IO Type.Type
primitiveType = intToken <|> realToken <|> boolToken <|> charToken <|> stringToken

listType :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
listType = try simpleListType <|> doubleListType

simpleListType :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
simpleListType = do
            a <- primitiveType
            b <- beginListConstToken
            c <- endListConstToken
            return (Type.List a [])

doubleListType :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
doubleListType = do
            a <- simpleListType
            b <- beginListConstToken
            c <- endListConstToken
            return (Type.List a [])

globalVars :: ParsecT [Token] ([(String, Type)], [Type]) IO [Type]
globalVars = try (do
        a <- beginScopeToken
        b <- globalToken
        c <- colonToken
        d <- varDeclarations
        e <- endScopeToken
        f <- globalToken
        e <- semiColonToken
        return d) <|> return []

-- concatenação de strings "string 1" + " string 2"
stringConcatExpression :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
stringConcatExpression = do
            a <- stringExpression
            b <- plusToken
            c <- stringExpression
            return (eval a b c)

-- Expressão envolvendo strings ou chars ou id
-- TODO: verificar o tipo de idToken
stringExpression :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
stringExpression = stringToken
               <|> charToken
            --    <|> idToken
               <|> stringConcatExpression

--parser para declaração de constante int
--constant int a = 10;
constantDecl :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
constantDecl = do
            const <- constantToken
            a <- typeToken
            b <- idToken
            c <- assignToken
            d <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
            e <- semiColonToken
            -- TODO: validar o tipo (gramática de atributos)
            -- TODO adicionar informação que é uma constante e não pode ser modificada
            updateState(symtableInsert (getIdData b, d))
            return (Type.Bool False) -- O ideal serial não retornar nada.

--int a = 10;
varInit :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
varInit = do
            a <- typeToken
            b <- idToken
            c <- assignToken
            d <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
            e <- semiColonToken
            -- TODO: validar o tipo (gramática de atributos)
            updateState(symtableInsert (getIdData b, d))
            return (Type.Bool False) -- O ideal serial não retornar nada.

varDeclarations :: ParsecT [Token] ([(String,Type)], [Type]) IO [Type]
varDeclarations = (do
                    c <- varDeclaration
                    e <- varDeclarations
                    return (c:e))
                    <|> return []

--int a;
varDeclaration :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
varDeclaration = do
            a <- typeToken
            b <- idToken
            c <- semiColonToken
            s <- getState
            updateState(symtableInsert (getIdData b, typeTableGet a s))
            return (typeTableGet a s) -- O ideal serial não retornar nada.

--a = 10;
varAssignment :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
varAssignment = do
            a <- idToken
            b <- assignToken
            c <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
            d <- semiColonToken
            -- TODO: validar o tipo (gramática de atributos)
            updateState(symtableUpdate (getIdData a, c))
            return (Type.Bool False) -- O ideal serial não retornar nada.

program :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
program = do
            a <- globalVars
            b <- structDeclarations
            s1 <- getState
            liftIO (print s1)
            eof
            return (Type.Bool False)

parser :: [Token] -> IO (Either ParseError Type)
parser = runParserT program ([], []) "Error message"
