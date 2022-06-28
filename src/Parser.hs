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

-- globalVars :: ParsecT [Token] MyState IO [Type]
-- globalVars = try (do
--         a <- beginScopeToken
--         b <- globalToken
--         c <- colonToken
--         d <- varCreations
--         e <- endScopeToken
--         f <- globalToken
--         e <- semiColonToken
--         return d) <|> return []

-- declaration :: ParsecT [Token] MyState IO [Type]
-- declaration = do
--         a <- beginScopeToken
--         try structDeclaration <|> mainFunction
--         --  <|> try functionCreation <|> subprogramCreation Nothing

-- declarations :: ParsecT [Token] MyState IO [Type]
-- declarations = (do
--                 c <- declaration
--                 e <- declarations
--                 return (c++e))
--                 <|> return []

-- structDeclaration :: ParsecT [Token] MyState IO [Type]
-- structDeclaration = do
--             b <- structToken
--             c <- idToken
--             d <- colonToken
--             (e, fields) <- fieldCreations
--             f <- endScopeToken
--             g <- idToken
--             s <- getState
--             if getIdData c /= getIdData g then fail "Nome da struct não é o mesmo"
--             else
--                 do
--                 h <- semiColonToken
--                 updateState(typeTableInsert (Type.Struct (getIdData c) fields))
--                 return [Type.Struct (getIdData c) fields]

-- fieldCreations :: ParsecT [Token] MyState IO ([Token],[(String, Type)])
-- fieldCreations = (do
--                     (c, f) <- fieldCreation
--                     (cs, fs) <- fieldCreations
--                     return (c++cs, f:fs))
--                     <|> return ([], [])

-- fieldCreation :: ParsecT [Token] MyState IO ([Token],(String, Type))
-- fieldCreation = do
--             (a, t) <- dataType
--             b <- idToken
--         --     (c, v) <- initialization a
--             return (a++[b],(getIdData b, t))

initialization :: Bool -> Type -> ParsecT [Token] MyState IO ([Token], Maybe Type)
initialization x t = try (do
        c <- assignToken
        (d, r) <- expr x
        -- e <- semiColonToken
        if x then 
                if compatible t (fromJust r) then return (c:d, r)
                else fail "tipos diferentes"
        else return (c:d, r))
        <|>
        (do
        -- e <- semiColonToken
        return ([],Just t))

dataType :: ParsecT [Token] MyState IO ([Token],Type)
dataType = do
        s <- getState
        t <- typeToken <|> idToken
        -- (r, v) <- listType (typeTableGet t s)
        return ([t], typeTableGet t s)

-- listType :: Type -> ParsecT [Token] MyState IO ([Token],Type)
-- listType t = try (do
--         b <- beginListConstToken
--         c <- endListConstToken
--         (r, v) <- listType (Type.List t [])
--         return (b:c:r, v)
--         -- melhorar isso aqui depois
--         ) <|> return ([], t)


-- -- -- concatenação de strings "string 1" + " string 2"
-- -- stringConcatExpression :: ParsecT [Token] MyState IO Type
-- -- stringConcatExpression = do
-- --             a <- stringExpression
-- --             b <- plusToken
-- --             c <- stringExpression
-- --             return (eval a b c)

-- -- Expressão envolvendo strings ou chars ou id
-- -- TODO: verificar o tipo de idToken
-- -- stringExpression :: ParsecT [Token] MyState IO [Token]
-- -- stringExpression = stringToken
-- --                <|> charToken
-- --                <|> idGetVal
-- --                <|> stringConcatExpression

-- --parser para declaração de constante int
-- --constant int a = 10;
-- -- constantDecl :: ParsecT [Token] MyState IO Type
-- -- constantDecl = do
-- --             const <- constantToken
-- --             -- TODO: usar essa informação sobre ser constante
-- --             a <- typeToken
-- --             b <- idToken
-- --             c <- assignToken
-- --             d <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
-- --             e <- semiColonToken
-- --             s <- getState
-- --             if not (compatible (typeTableGet a s) d) then fail "tipos diferentes" else
-- --                 do
-- --                 updateState(symtableInsert (getIdData b, d))
-- --                 return (Type.Bool False) -- O ideal seria não retornar nada.

varCreation :: Bool -> ParsecT [Token] MyState IO [Token]
varCreation x = do
            (a, t) <- dataType
            b <- idToken
            (c, r) <- initialization x t
            if not (compatible t (fromJust r)) then fail "tipos diferentes" else
                if x then
                        do
                        s <- getState
                        updateState(symtableInsert (getIdData b, fromJust r))
                        return (a++[b]++c)
                else
                        return (a++[b]++c)

-- varCreations :: ParsecT [Token] MyState IO [Type]
-- varCreations = (do
--                     c <- varCreation -- <|> constantDecl
--                     e <- varCreations
--                     return (c:e))
--                     <|> return []

--a = 10;
varAssignment :: Bool -> ParsecT [Token] MyState IO [Token]
varAssignment x = do
                a <- idToken
                b <- assignToken
                (c, v) <- expr x
                d <- semiColonToken
                -- TODO: validar o tipo (gramática de atributos)
                if x then 
                        do 
                        updateState(symtableUpdate (getIdData a, fromJust v))
                        return ([a]++[b]++c++[d])
                else return ([a]++[b]++c++[d])

-- functionCreation :: ParsecT [Token] MyState IO Type
-- functionCreation = do
--         b <- dataType
--         subprogramCreation (Just b)

-- subprogramCreation :: Maybe Type -> ParsecT [Token] MyState IO Type
-- subprogramCreation ret = do
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
--                 do
--                 -- TODO: atualizar a lista de comandos !!!
--                 updateState(subsprogramTableInsert (getIdData c, ret, e, [c]))
--                 return (Type.Bool False)

-- params :: ParsecT [Token] MyState IO [(String, Type)]
-- params = (do
--         c <- try param <|> refParam
--         e <- params
--         return (c:e))
--         <|> return []

-- param :: ParsecT [Token] MyState IO (String, Type)
-- param = do
--         a <- dataType
--         b <- idToken
--         return (getIdData b, a)

-- refParam :: ParsecT [Token] MyState IO (String, Type)
-- refParam = do
--         ref <- refToken
--         a <- dataType
--         b <- idToken
--         return (getIdData b, a)

-- returnCall :: ParsecT [Token] MyState IO [Type]
-- returnCall = do
--         -- TODO: pensar em como voltar para o lugar que chamou a função e devolver esse valor
--         a <- returnToken
--         a <- expression
--         return [Type.Bool True]
--         -- expression

-- destroyCall :: ParsecT [Token] MyState IO [Type]
-- destroyCall = do
--         a <- destroyToken
--         b <- idToken
--         -- remover id da heap
--         return [Type.Bool True]

-- statements :: ParsecT [Token] MyState IO [Type]
-- statements = (do
--         c <- statement
--         d <- semiColonToken
--         e <- statements
--         -- s <- getState
--         -- liftIO (print s)
--         return (c++e))
--         <|> return []

-- statement :: ParsecT [Token] MyState IO [Type]
-- statement = try varCreations
--         <|> try varAssignment
--         -- <|> try conditional
--         -- <|> try loop
--         -- <|> try procedureCall
--         -- <|> try returnCall
--         -- <|> try destroyCall
--         -- <|> try continueToken
--         -- <|> breakToken

expr :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
expr x = try (do
        (t1, n1) <- term x
        op <- plusToken <|> minusToken
        (t2, n2) <- expr x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> term x

term :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
term x = try (do
        (t1, n1) <- fator x
        op <- multToken <|> divToken <|> modToken
        (t2, n2) <- term x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> fator x

fator :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
fator x
        | x = (do
                t <- intToken
                return ([t], Just (toType t)))
                <|>
                (do
                t <- realToken
                return ([t], Just (toType t)))
        | otherwise = (do
                t <- intToken
                return ([t], Nothing))
                <|>
                (do
                t <- realToken
                return ([t], Nothing))

-- idGetVal :: ParsecT [Token] MyState IO Type
-- idGetVal = do
--         a <- idToken
--         s <- getState
--         return (symtableGet (getIdData a) s)


-- casting :: ParsecT [Token] MyState IO Token
-- casting = castingBoolToken <|> castingIntToken <|> castingRealToken <|> castingCharToken <|> castingStringToken

-- literal :: ParsecT [Token] MyState IO Token
-- literal = boolToken <|> intToken<|> realToken <|> charToken <|> stringToken

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
        -- a <- globalVars
        b <- varCreation True
        -- c <- expr
        eof
        -- return c
        s <- getState
        liftIO (print s)
        return []
        -- return (runFunc (getMainFunc s) [])

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program ([], [], []) "Error message"
