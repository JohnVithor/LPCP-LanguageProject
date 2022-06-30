module Expressions where
import Lexer
import Type
import Data.Maybe
import Text.Parsec
import Control.Monad.IO.Class
import SymTable
import TokenParser
import Eval

--TODO: campos de structs, acesso a array, chamada de função, casting 

-- Parser geral de expressões
expression :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
expression x = try (numExpr x) <|> logExpr x

-- Parser inicial para expressões numéricas (soma e subtração)
numExpr :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
numExpr x = try (do
        (t1, n1) <- numTerm x
        op <- plusToken <|> minusToken
        (t2, n2) <- numExpr x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> numTerm x

-- Parser secundário para expressões numéricas (multiplicação, divisão e módulo)
numTerm :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
numTerm x = try (do
        (t1, n1) <- numFactor x
        op <- multToken <|> divToken <|> modToken
        (t2, n2) <- numTerm x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> numFactor x

-- Parser final para expressões numéricas (literais, variáveis, parênteses, etc)
numFactor :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
numFactor x = try (do
                (tk,tp) <- intToken<|> realToken
                return ([tk], Just tp)
                ) <|> try (do 
                a <- idToken
                s <- getState
                if x then return ([a], Just (symtableGet (getIdData a) s))
                else return ([a],Nothing)
                ) <|> try (do 
                a <- beginExpressionToken 
                (tk,tp) <- numExpr x
                c <- endExpressionToken 
                s <- getState
                if x then return ([a] ++ tk ++ [c], tp)
                else return ([a] ++ tk ++ [c],Nothing)
                )
-- Parser inicial para expressões lógicas (OR)
logExpr :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
logExpr x = try (do
        (t1, n1) <- logTerm1 x
        op <- orToken
        (t2, n2) <- logExpr x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> logTerm1 x

-- Parser secundário para expressões lógicas (AND)
logTerm1 :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
logTerm1 x = try (do
        (t1, n1) <- logTerm2 x
        op <- andToken 
        (t2, n2) <- logTerm1 x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> logTerm2 x

-- Parser ternário para expressões lógicas (NOT)
logTerm2 :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
logTerm2 x = try (do
        op <- notToken 
        (t1, n1) <- logFactor x
        if x then return (op : t1,Just (evalUni op (fromJust n1)))
        else return (op : t1, Nothing))
        <|> logFactor x

-- Parser final para expressões lógicas (booleano, variáveis, parênteses, etc)
logFactor :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
logFactor x =   try (do
                (tk,tp) <- boolToken
                return ([tk], Just tp)
                ) <|> try (do 
                a <- idToken
                s <- getState
                if x then return ([a], Just (symtableGet (getIdData a) s))
                else return ([a],Nothing)
                ) <|> try (do 
                a <- beginExpressionToken 
                (tk,tp) <- logExpr x
                c <- endExpressionToken 
                s <- getState
                if x then return ([a] ++ tk ++ [c], tp)
                else return ([a] ++ tk ++ [c],Nothing)
                )
                <|> try (do 
                (tk,tp) <- comparison x
                s <- getState
                if x then return (tk, tp)
                else return (tk,Nothing)
                )

                -- IN
                -- IS
                -- COMPARAÇÕES


        -- Os operadores '==' e '!=' podem trabalhar com strings
                -- Utilizei apenas numExpr nos trys desses dois operadores
                --Falta implementar stringExpr
comparison :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
comparison x = try (do
                (t1, n1) <- numExpr x
                op <- lessOrEqualToken 
                (t2, n2) <- numExpr x
                if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
                else return (t1 ++ [op] ++ t2, Nothing)
                ) <|> 
                try (do
                (t1, n1) <- numExpr x
                op <- greaterOrEqualToken 
                (t2, n2) <- numExpr x
                if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
                else return (t1 ++ [op] ++ t2, Nothing)
                ) <|> 
                try (do
                (t1, n1) <- numExpr x
                op <- equalToken 
                (t2, n2) <- numExpr x
                if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
                else return (t1 ++ [op] ++ t2, Nothing)
                ) <|> 
                try (do
                (t1, n1) <- numExpr x
                op <- notEqualToken 
                (t2, n2) <- numExpr x
                if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
                else return (t1 ++ [op] ++ t2, Nothing)
                ) <|> 
                try (do
                (t1, n1) <- numExpr x
                op <- lessToken 
                (t2, n2) <- numExpr x
                if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
                else return (t1 ++ [op] ++ t2, Nothing)
                ) <|> 
                try (do
                (t1, n1) <- numExpr x
                op <- greaterToken 
                (t2, n2) <- numExpr x
                if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
                else return (t1 ++ [op] ++ t2, Nothing)
                )
        

        