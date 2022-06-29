module Statements where
import Lexer
import Type
import Data.Maybe
import Text.Parsec
import Control.Monad.IO.Class
import SymTable
import TokenParser
import Eval
import DataType
import Expressions


initialization :: Bool -> Type -> ParsecT [Token] MyState IO ([Token], Maybe Type)
initialization x t = try (do
        c <- assignToken
        (d, r) <- expression x
        -- e <- semiColonToken
        if x then
                if compatible t (fromJust r) then return (c:d, r)
                else fail "tipos diferentes"
        else return (c:d, r))
        <|>
        (do
        -- e <- semiColonToken
        return ([],Just t))

varCreation :: Bool -> ParsecT [Token] MyState IO [Token]
varCreation x = do
            (a, t) <- dataType
            b <- idToken
            (c, r) <- initialization x t
            if x then
                if not (compatible t (fromJust r)) then fail "tipos diferentes" else
                        do
                        s <- getState
                        updateState(symtableInsert (getIdData b, fromJust r))
                        return (a++[b]++c)
                else
                        return (a++[b]++c)

varCreations :: Bool -> ParsecT [Token] MyState IO [Token]
varCreations x = (do
                    c <- varCreation x -- <|> constantDecl
                    e <- varCreations x
                    return (c++e))
                    <|> return []

varAssignment :: Bool -> ParsecT [Token] MyState IO [Token]
varAssignment x = do
                a <- idToken
                b <- assignToken
                (c, v) <- expression x
                d <- semiColonToken
                -- TODO: validar o tipo (gramática de atributos)
                if x then
                        do
                        updateState(symtableUpdate (getIdData a, fromJust v))
                        return ([a]++[b]++c++[d])
                else return ([a]++[b]++c++[d])

returnCall :: Bool -> ParsecT [Token] MyState IO ([Token],Type)
returnCall x = do
        -- TODO: pensar em como voltar para o lugar que chamou a função e devolver esse valor
        a <- returnToken
        b <- expression x
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
        -- <|> try returnCall
        -- <|> try destroyCall
        -- <|> try continueToken
        -- <|> breakToken