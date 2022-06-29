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
import GHC.Exception (fromCallSiteList)
import Expressions
import Statements

globalVars :: ParsecT [Token] MyState IO [Token]
globalVars = try (do
        a <- beginScopeToken
        b <- globalToken
        c <- colonToken
        d <- globalVarCreations
        e <- endScopeToken
        f <- globalToken
        g <- semiColonToken
        return (a:b:c:e:d ++ f:[g])) <|> return []

globalVarCreations :: ParsecT [Token] MyState IO [Token]
globalVarCreations = try (do
                a <- varCreation True -- <|> constantDecl
                b <- semiColonToken 
                c <- globalVarCreations
                return (a++b:c))
                <|> return []

-- -- concatenação de strings "string 1" + " string 2"
-- stringConcatExpression :: ParsecT [Token] MyState IO Type
-- stringConcatExpression = do
--             a <- stringExpression
--             b <- plusToken
--             c <- stringExpression
--             return (eval a b c)

-- Expressão envolvendo strings ou chars ou id
-- TODO: verificar o tipo de idToken
-- stringExpression :: ParsecT [Token] MyState IO [Token]
-- stringExpression = stringToken
--                <|> charToken
--                <|> idGetVal
--                <|> stringConcatExpression

--parser para declaração de constante int
--constant int a = 10;
-- constantDecl :: ParsecT [Token] MyState IO Type
-- constantDecl = do
--             const <- constantToken
--             -- TODO: usar essa informação sobre ser constante
--             a <- typeToken
--             b <- idToken
--             c <- assignToken
--             d <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
--             e <- semiColonToken
--             s <- getState
--             if not (compatible (typeTableGet a s) d) then fail "tipos diferentes" else
--                 do
--                 updateState(symtableInsert (getIdData b, d))
--                 return (Type.Bool False) -- O ideal seria não retornar nada.

--a = 10;








-- casting :: ParsecT [Token] MyState IO Token
-- casting = castingBoolToken <|> castingIntToken <|> castingRealToken <|> castingCharToken <|> castingStringToken

literal :: ParsecT [Token] MyState IO (Token,Type)
literal = intToken<|> realToken <|> charToken <|> stringToken <|> boolToken

-- mainFunction :: ParsecT [Token] MyState IO [Type]
-- mainFunction = do
--         b <- typeToken
--         c <- idToken
--         d <- beginExpressionToken
--         e <- params
--         f <- endExpressionToken
--         g <- colonToken
--         h <- statements
--         i <- endScopeToken
--         j <- idToken
--         l <- semiColonToken
--         if getIdData c /= getIdData j then fail "Nome do subprograma não é o mesmo"
--         else
--                 if getIdData c /= "main" then fail "função main não encontrada" else
--                 do
--                 -- TODO: atualizar a lista de comandos !!!
--                 updateState(subsprogramTableInsert (getIdData c, Just (Type.Int 0), e, [c]))
--                 return h

program :: ParsecT [Token] MyState IO [Token]
program = do
        a <- globalVars
        b <- statements True
        eof
        s <- getState
        liftIO (print s)
        return []
        -- return (runFunc (getMainFunc s) [])

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program ([], [], []) "Error message"
