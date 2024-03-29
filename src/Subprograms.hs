module Subprograms where
import Lexer
import Type
import Text.Parsec
import SymTable
import TokenParser
import Statements

functionCreation :: ParsecT [Token] MyState IO [Token]
functionCreation = do
        (tk, tp, _) <- dataType
        s <- subprogramCreation (Just tp)
        let r = checkReturn s
        if r then return (tk++s)
        -- Melhorar isso aqui para buscar em todos os caminhos do código
        else error ("Retorno não encontrado na função '"++getIdData (head s)++"'")

checkReturn :: [Token] -> Bool
checkReturn [] = False 
checkReturn ((Return _):_) = True 
checkReturn (_:tks) = checkReturn tks

subprogramCreation :: Maybe Type -> ParsecT [Token] MyState IO [Token]
subprogramCreation ret = do
        c <- idToken
        d <- beginExpressionToken
        (e,ps) <- params
        f <- endExpressionToken
        g <- colonToken
        (h,_) <- statements False
        i <- endScopeToken
        j <- idToken
        l <- semiColonToken
        if getIdData c /= getIdData j then fail "Nome utilizad na declaração do subprograma não é o mesmo"
        else
                do
                updateState(subsprogramTableInsert (getIdData c, ret, ps, h))
                return (c:d:e++f:g:h++i:j:[l])

params :: ParsecT [Token] MyState IO ([Token],[(String, Type, Bool)])
params = try (do
        (tk, tp) <- param
        c <- commaToken 
        (tks, ps) <- params
        return (tk++c:tks,tp:ps))
        <|> do 
                (tk, tp) <- param
                return (tk,[tp])
        <|> return ([],[])

param :: ParsecT [Token] MyState IO ([Token],(String, Type, Bool))
param = do
        (tk, tp, c) <- dataType
        b <- idToken
        return (tk, (getIdData b, tp, c))