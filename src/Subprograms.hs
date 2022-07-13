module Subprograms where
import Lexer
import Type
import Text.Parsec
import SymTable
import TokenParser
import Declarations
import Statements
import Control.Monad.IO.Class (MonadIO(liftIO))

functionCreation :: ParsecT [Token] MyState IO [Token]
functionCreation = do
        (tk, tp) <- dataType
        s <- subprogramCreation (Just tp)
        let r = checkReturn s
        if r then return (tk++s)
        -- Melhorar isso aqui para buscar em todos os caminhos do código
        else error "Retorno não encontrado"

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
        if getIdData c /= getIdData j then fail "Nome do subprograma não é o mesmo"
        else
                do
                updateState(subsprogramTableInsert (getIdData c, ret, ps, h))
                return (c:d:e++f:g:h++i:j:[l])

params :: ParsecT [Token] MyState IO ([Token],[(String, Type)])
params = try (do
        (tk, tp) <- param-- <|> refParam
        c <- commaToken 
        (tks, ps) <- params
        return (tk++c:tks,tp:ps))
        <|> do 
                (tk, tp) <- param
                return (tk,[tp])
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