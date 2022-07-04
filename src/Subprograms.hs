module Subprograms where
import Lexer
import Type
import Text.Parsec
import SymTable
import TokenParser
import Declarations
import Statements

functionCreation :: ParsecT [Token] MyState IO [Token]
functionCreation = do
        (tk, tp) <- dataType
        s <- subprogramCreation (Just tp)
        return (tk++s)

subprogramCreation :: Maybe Type -> ParsecT [Token] MyState IO [Token]
subprogramCreation ret = do
        c <- idToken
        d <- beginExpressionToken
        (e,ps) <- params
        f <- endExpressionToken
        g <- colonToken
        h <- statements False
        i <- endScopeToken
        j <- idToken
        l <- semiColonToken
        if getIdData c /= getIdData j then fail "Nome do subprograma não é o mesmo"
        else
                do
                updateState(subsprogramTableInsert (getIdData c, ret, ps, h))
                return (c:d:e++f:g:h++i:j:[l])

params :: ParsecT [Token] MyState IO ([Token],[(String, Type)])
params = (do
        (tk, tp) <- param-- <|> refParam
        (tks, ps) <- params
        return (tk++tks,tp:ps))
        <|> return ([],[])

param :: ParsecT [Token] MyState IO ([Token],(String, Type))
param = do
        (tk, tp) <- dataType
        b <- idToken
        return (tk, (getIdData b, tp))

refParam :: ParsecT [Token] MyState IO ([Token],(String, Type))
refParam = do
        ref <- refToken
        -- TODO: usar o ref de algum modo
        (tk, tp) <- dataType
        b <- idToken
        return (ref:tk++[b],(getIdData b, tp))