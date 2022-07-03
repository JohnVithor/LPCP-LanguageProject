module Subprograms where
import Lexer
import Type
import Text.Parsec
import SymTable
import TokenParser
import Declarations
import Statements

functionCreation :: ParsecT [Token] MyState IO Type
functionCreation = do
        (tk, tp) <- dataType
        subprogramCreation (Just tp)

subprogramCreation :: Maybe Type -> ParsecT [Token] MyState IO Type
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
                -- TODO: atualizar a lista de comandos !!!
                updateState(subsprogramTableInsert (getIdData c, ret, ps, [c]))
                return (Type.Bool False)

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

refParam :: ParsecT [Token] MyState IO (String, Type)
refParam = do
        ref <- refToken
        (tk, tp) <- dataType
        b <- idToken
        return (getIdData b, tp)