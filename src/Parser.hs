{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Eval
import TokenParser
import SymTable
import Type
import Data.Maybe

globalVars :: ParsecT [Token] MyState IO [Type]
globalVars = try (do
        a <- beginScopeToken
        b <- globalToken
        c <- colonToken
        d <- varCreations
        e <- endScopeToken
        f <- globalToken
        e <- semiColonToken
        return d) <|> return []

structDeclarations :: ParsecT [Token] MyState IO [Type]
structDeclarations = (do
                    c <- structDeclaration
                    e <- try structDeclarations <|> functionCreations
                    return (c:e))
                    <|> return []

structDeclaration :: ParsecT [Token] MyState IO Type
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

fieldCreations :: ParsecT [Token] MyState IO [(String, Type)]
fieldCreations = (do
                    c <- fieldCreation
                    e <- fieldCreations
                    return (c:e))
                    <|> return []

fieldCreation :: ParsecT [Token] MyState IO (String, Type)
fieldCreation = do
            a <- dataType
            s <- getState
            b <- idToken
            c <- initialization a
            return (getIdData b, c)

initialization :: Type -> ParsecT [Token] MyState IO Type
initialization t = try (do
        c <- assignToken
        d <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
        e <- semiColonToken
        if compatible t d then return d else fail "tipos diferentes")
        <|>
        (do
        e <- semiColonToken
        return t)

dataType :: ParsecT [Token] MyState IO Type
dataType = do
        s <- getState
        t <- typeToken <|> idToken
        listType (typeTableGet t s)

listType :: Type -> ParsecT [Token] MyState IO Type
listType t = try (do
        b <- beginListConstToken
        c <- endListConstToken
        listType (Type.List t [])
        -- melhorar isso aqui depois
        ) <|> return t


-- concatenação de strings "string 1" + " string 2"
stringConcatExpression :: ParsecT [Token] MyState IO Type
stringConcatExpression = do
            a <- stringExpression
            b <- plusToken
            c <- stringExpression
            return (eval a b c)

-- Expressão envolvendo strings ou chars ou id
-- TODO: verificar o tipo de idToken
stringExpression :: ParsecT [Token] MyState IO Type
stringExpression = stringToken
               <|> charToken
            --    <|> idToken
               <|> stringConcatExpression

--parser para declaração de constante int
--constant int a = 10;
constantDecl :: ParsecT [Token] MyState IO Type
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

varCreation :: ParsecT [Token] MyState IO Type
varCreation = do
            a <- dataType
            s <- getState
            b <- idToken
            c <- initialization a
            if not (compatible a c) then fail "tipos diferentes" else 
                do
                updateState(symtableInsert (getIdData b, c))
                return (Type.Bool False) -- O ideal seria não retornar nada.

varCreations :: ParsecT [Token] MyState IO [Type]
varCreations = (do
                    c <- varCreation
                    e <- varCreations
                    return (c:e))
                    <|> return []

--a = 10;
varAssignment :: ParsecT [Token] MyState IO Type
varAssignment = do
            a <- idToken
            b <- assignToken
            c <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
            d <- semiColonToken
            -- TODO: validar o tipo (gramática de atributos)
            updateState(symtableUpdate (getIdData a, c))
            return (Type.Bool False) -- O ideal seria não retornar nada.

functionCreations :: ParsecT [Token] MyState IO [Type]
functionCreations = (do
                    c <- functionCreation
                    e <- functionCreations
                    return (c:e))
                    <|> return []

functionCreation :: ParsecT [Token] MyState IO Type
functionCreation = do
        a <- beginScopeToken 
        b <- dataType 
        c <- idToken 
        d <- beginExpressionToken
        e <- params
        f <- endExpressionToken
        g <- colonToken 
        -- h <- statements
        i <- endScopeToken 
        j <- idToken 
        l <- semiColonToken 
        if getIdData c /= getIdData j then fail "Nome da função não é o mesmo"
        else
                do 
                -- TODO: atualizar a lista de comandos !!!
                updateState(subsprogramTableInsert (getIdData c, Just b, e, [c]))
                return (Type.Bool False)


params :: ParsecT [Token] MyState IO [(String, Type)]
params = (do
        c <- param
        e <- params
        return (c:e))
        <|> return []

param :: ParsecT [Token] MyState IO (String, Type)
param = do 
        a <- dataType
        b <- idToken 
        return (getIdData b, a)

program :: ParsecT [Token] MyState IO Type
program = do
            a <- globalVars
            b <- try structDeclarations <|> functionCreations
            s1 <- getState
            liftIO (print s1)
            eof
            return (Type.Bool False)

parser :: [Token] -> IO (Either ParseError Type)
parser = runParserT program ([], [], []) "Error message"
