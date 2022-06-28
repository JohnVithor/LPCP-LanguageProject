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

declaration :: ParsecT [Token] MyState IO [Type]
declaration = do
        a <- beginScopeToken
        try structDeclaration <|> mainFunction
        --  <|> try functionCreation <|> subprogramCreation Nothing

declarations :: ParsecT [Token] MyState IO [Type]
declarations = (do
                c <- declaration
                e <- declarations
                return (c++e))
                <|> return []

structDeclaration :: ParsecT [Token] MyState IO [Type]
structDeclaration = do
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
                return [Type.Bool False]

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
        d <- expr
        -- e <- semiColonToken
        if compatible t d then return d else fail "tipos diferentes")
        <|>
        (do
        -- e <- semiColonToken
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
               <|> idGetVal
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
                return c -- O ideal seria não retornar nada.

varCreations :: ParsecT [Token] MyState IO [Type]
varCreations = (do
                    c <- varCreation -- <|> constantDecl
                    e <- varCreations
                    return (c:e))
                    <|> return []

--a = 10;
varAssignment :: ParsecT [Token] MyState IO [Type]
varAssignment = do
            a <- idToken
            b <- assignToken
            c <- expression
            d <- semiColonToken
            -- TODO: validar o tipo (gramática de atributos)
            updateState(symtableUpdate (getIdData a, c))
            return [Type.Bool False] -- O ideal seria não retornar nada.

functionCreation :: ParsecT [Token] MyState IO Type
functionCreation = do
        b <- dataType
        subprogramCreation (Just b)

subprogramCreation :: Maybe Type -> ParsecT [Token] MyState IO Type
subprogramCreation ret = do
        c <- idToken
        d <- beginExpressionToken
        e <- params
        f <- endExpressionToken
        g <- colonToken 
        h <- statements
        i <- endScopeToken 
        j <- idToken 
        l <- semiColonToken 
        if getIdData c /= getIdData j then fail "Nome do subprograma não é o mesmo"
        else
                do
                -- TODO: atualizar a lista de comandos !!!
                updateState(subsprogramTableInsert (getIdData c, ret, e, [c]))
                return (Type.Bool False)

params :: ParsecT [Token] MyState IO [(String, Type)]
params = (do
        c <- try param <|> refParam
        e <- params
        return (c:e))
        <|> return []

param :: ParsecT [Token] MyState IO (String, Type)
param = do
        a <- dataType
        b <- idToken
        return (getIdData b, a)

refParam :: ParsecT [Token] MyState IO (String, Type)
refParam = do
        ref <- refToken
        a <- dataType
        b <- idToken
        return (getIdData b, a)

returnCall :: ParsecT [Token] MyState IO [Type]
returnCall = do
        -- TODO: pensar em como voltar para o lugar que chamou a função e devolver esse valor
        a <- returnToken
        a <- expression
        return [Type.Bool True]
        -- expression

destroyCall :: ParsecT [Token] MyState IO [Type]
destroyCall = do
        a <- destroyToken
        b <- idToken
        -- remover id da heap
        return [Type.Bool True]

statements :: ParsecT [Token] MyState IO [Type]
statements = (do
        c <- statement
        d <- semiColonToken 
        e <- statements
        -- s <- getState
        -- liftIO (print s)
        return (c++e))
        <|> return []

statement :: ParsecT [Token] MyState IO [Type]
statement = try varCreations
        <|> try varAssignment
        -- <|> try conditional
        -- <|> try loop
        -- <|> try procedureCall
        <|> try returnCall
        <|> try destroyCall
        -- <|> try continueToken
        -- <|> breakToken

negativeNumber :: ParsecT [Token] MyState IO Type
negativeNumber = do
        a <- minusToken
        b <- beginExpressionToken
        c <- numericExpression
        d <- endExpressionToken
        return (evalUni a c)

numericExpression :: ParsecT [Token] MyState IO Type
numericExpression = do
        a <- intToken
        b <- plusToken
        c <- realToken
        return (eval a b c)

expr :: ParsecT [Token] MyState IO Type
expr = try (do
        n1 <- term
        op <- plusToken <|> minusToken
        n2 <- expr
        return (eval n1 op n2)) 
        <|> term

term :: ParsecT [Token] MyState IO Type
term = try (do
        n1 <- fator
        op <- multToken <|> divToken <|> modToken
        n2 <- term
        return (eval n1 op n2)) 
        <|> fator

fator :: ParsecT [Token] MyState IO Type
fator = intToken <|> realToken <|> (do
        a <- beginExpressionToken 
        b <- expr
        c <- endExpressionToken 
        return b)

idGetVal :: ParsecT [Token] MyState IO Type
idGetVal = do 
        a <- idToken
        s <- getState
        return (symtableGet (getIdData a) s)
        

expression :: ParsecT [Token] MyState IO Type
expression = literal <|> idGetVal <|> (do
        a <- casting
        b <- expression
        return (cast a b)
        ) <|> stringExpression

casting :: ParsecT [Token] MyState IO Token
casting = castingBoolToken <|> castingIntToken <|> castingRealToken <|> castingCharToken <|> castingStringToken

literal :: ParsecT [Token] MyState IO Type
literal = boolToken <|> intToken<|> realToken <|> charToken <|> stringToken

mainFunction :: ParsecT [Token] MyState IO [Type]
mainFunction = do
        b <- typeToken
        c <- idToken 
        d <- beginExpressionToken
        e <- params
        f <- endExpressionToken
        g <- colonToken 
        h <- statements
        i <- endScopeToken 
        j <- idToken 
        l <- semiColonToken 
        if getIdData c /= getIdData j && getIdData c /= "main" then fail "Nome do subprograma não é o mesmo"
        else
                do 
                -- TODO: atualizar a lista de comandos !!!
                updateState(subsprogramTableInsert (getIdData c, Just (Type.Int 0), e, [c]))
                return h

program :: ParsecT [Token] MyState IO [Type]
program = do
        -- a <- globalVars
        b <- declarations
        -- c <- expr
        eof
        -- return c
        s <- getState 
        return b
        -- return (runFunc (getMainFunc s) [])

parser :: [Token] -> IO (Either ParseError [Type])
parser = runParserT program ([], [], []) "Error message"
