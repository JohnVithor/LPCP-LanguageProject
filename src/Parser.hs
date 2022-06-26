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

globalVars :: ParsecT [Token] ([(String, Type)], [Type]) IO [Type]
globalVars = try (do
        a <- beginScopeToken
        b <- globalToken
        c <- colonToken
        d <- varCreations
        e <- endScopeToken
        f <- globalToken
        e <- semiColonToken
        return d) <|> return []

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
            if getIdData c /= getIdData g then fail "Nome da struct não é o mesmo"
            else
                do
                h <- semiColonToken
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
            a <- dataType
            s <- getState
            b <- idToken
            c <- initialization a
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

dataType :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
dataType = do
        s <- getState
        t <- typeToken <|> idToken
        listType (typeTableGet t s)

listType :: Type -> ParsecT [Token] ([(String,Type)], [Type]) IO Type
listType t = try (do
        b <- beginListConstToken
        c <- endListConstToken
        listType (Type.List t [])
        -- melhorar isso aqui depois
        ) <|> return t


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
            -- TODO: usar essa informação sobre ser constante
            a <- typeToken 
            b <- idToken
            c <- assignToken
            d <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
            e <- semiColonToken
            s <- getState 
            if not (compatible (typeTableGet a s) d) then fail "tipos diferentes" else 
                do
                updateState(symtableInsert (getIdData b, d))
                return (Type.Bool False) -- O ideal seria não retornar nada.

varCreation :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
varCreation = do
            a <- dataType
            s <- getState
            b <- idToken
            c <- initialization a
            if not (compatible a c) then fail "tipos diferentes" else 
                do
                updateState(symtableInsert (getIdData b, c))
                return (Type.Bool False) -- O ideal seria não retornar nada.

varCreations :: ParsecT [Token] ([(String,Type)], [Type]) IO [Type]
varCreations = (do
                    c <- varCreation
                    e <- varCreations
                    return (c:e))
                    <|> return []

--a = 10;
varAssignment :: ParsecT [Token] ([(String,Type)], [Type]) IO Type
varAssignment = do
            a <- idToken
            b <- assignToken
            c <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
            d <- semiColonToken
            -- TODO: validar o tipo (gramática de atributos)
            updateState(symtableUpdate (getIdData a, c))
            return (Type.Bool False) -- O ideal seria não retornar nada.

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
