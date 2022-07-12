module Expressions where
import Lexer
import Type
import Data.Maybe
import Text.Parsec
import SymTable
import TokenParser
import Eval

--TODO: campos de structs, acesso a array, chamada de função, casting 

-- Parser geral de expressões
expression :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
expression x = try (do
                (a,r) <- numExpr x
                return (a,r))
        <|> try (do
                (a,r) <- logExpr x
                return (a,r))
        <|> try (do
                (a,r) <- stringExpr x
                return (a,r))
        <|> ( do 
                a <- castingToken
                (b, v) <- expression x
                if x then return (a:b, Just (cast a (fromJust v)))
                else return (a:b, Nothing)
        )

castingToken :: ParsecT [Token] u IO Token
castingToken = castingBoolToken
        <|> castingIntToken
        <|> castingRealToken 
        <|> castingCharToken 
        <|> castingStringToken  

-- Parser inicial para expressões numéricas (soma e subtração)
numExpr :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
numExpr x = try (do
        (t1, n1) <- numTerm x
        (t2, n2) <- evalRemaining x n1
        if x then return (t1 ++ t2, n2)
        else return (t1 ++ t2, Nothing))
        <|> numTerm x

evalRemaining :: Bool -> Maybe Type -> ParsecT [Token] MyState IO ([Token],Maybe Type)
evalRemaining x n1 = try(do
                op <- plusToken <|> minusToken
                (t2, n2) <- numTerm x
                if x then do
                        let e = eval (fromJust n1) op (fromJust n2)
                        (t3, r) <- evalRemaining x (Just e)
                        return (op:t2++t3, r) 
                else do
                        (t3, r) <- evalRemaining x Nothing
                        return (op:t2++t3, r))
                <|> return ([], n1)

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
                (tk,tp) <- intToken <|> realToken
                return ([tk], Just tp)
                ) <|> try (getVar x False)
                <|> try ( do
                a <- minusToken 
                (b, v) <- numFactor x
                if x then do return (a:b, Just (evalUni a (fromJust v)))
                else return (a:b, Nothing )
                ) <|> (do
                a <- beginExpressionToken
                (tk,tp) <- numExpr x
                c <- endExpressionToken
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

-- Parser final para expressões lógicas (booleano, variáveis, parênteses, etc) -
logFactor :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
logFactor x =   try (do
                (tk,tp) <- comparison x
                if x then return (tk, tp)
                else return (tk,Nothing)
                ) <|> try (do
                (tk,tp) <- boolToken
                return ([tk], Just tp)
                ) <|> try (getVar x False) <|> try (do
                a <- beginExpressionToken
                (tk,tp) <- logExpr x
                c <- endExpressionToken
                if x then return ([a] ++ tk ++ [c], tp)
                else return ([a] ++ tk ++ [c],Nothing)
                )
                -- IN
                -- IS

getVar :: Bool -> Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
getVar x y = try (do
        a <- idToken
        s <- getState
        if x then do
                if y then do 
                        let (_, oldValue, _) = symtableGetVar (getIdData a) s
                        (_, _, _, ov) <- dotAccess x (Just oldValue)
                        return ([a], ov)
                else do
                        let (_, oldValue, _) = symtableGetValue (getIdData a) s
                        (_, _, _, ov) <- dotAccess x (Just oldValue)
                        return ([a], ov)
        else do
                (b, _, _, _) <- dotAccess x Nothing
                return (a:b,Nothing)
        )

dotAccess :: Bool -> Maybe Type -> ParsecT [Token] MyState IO ([Token], [Maybe Type], [String] ,Maybe Type)
dotAccess x v = (do
        b <- dotToken
        c <- idToken
        if x then do
                let name = getIdData c
                let tv = fromJust v
                if isRefType (fromJust v) then do
                        s <- getState
                        let (_,refV,_) = symtableGetInner2 (getRefKey tv) (getSymbolTbl s)
                        let fieldValue = getStructField refV name
                        (d, sts, ns, vv) <- dotAccess x (Just fieldValue)
                        return ([b,c]++d, Just refV:sts, name:ns, vv)
                else do
                        let fieldValue = getStructField tv name
                        (d, s, ns, vv) <- dotAccess x (Just fieldValue)
                        return ([b,c]++d, v:s, name:ns, vv)
        else do
                (d, s, ns, vv) <- dotAccess x v
                return ([b,c]++d, v:s, "":ns, vv)
        ) <|> do
                return ([],[],[],v)

-- Os operadores '==' e '!=' podem trabalhar com strings
-- Utilizei apenas numExpr nos trys desses dois operadores
--Falta implementar stringExpr
comparison :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
comparison x = do
                (t1, n1) <- try (numExpr x) <|> stringExpr x
                op <- comparisonOp
                (t2, n2) <- try (numExpr x) <|> stringExpr x
                if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
                else return (t1 ++ [op] ++ t2, Nothing)


comparisonOp :: ParsecT [Token] MyState IO Token
comparisonOp = equalToken
                <|> notEqualToken
                <|> lessOrEqualToken
                <|> greaterOrEqualToken
                <|> greaterToken
                <|> lessToken

stringExpr :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
stringExpr x = try (do
        (t1, n1) <- stringFactor x
        op <- plusToken
        (t2, n2) <- stringExpr x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> stringFactor x

-- Expressão envolvendo strings ou chars ou id
-- TODO: verificar o tipo de idToken
stringFactor :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
stringFactor x = try (do 
                (tk, tp) <- stringToken <|> charToken
                return ([tk], Just tp))
               <|> getVar x False