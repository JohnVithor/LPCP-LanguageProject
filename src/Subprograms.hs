module Subprograms where
import Lexer
import Type
import Data.Maybe
import Text.Parsec
import Control.Monad.IO.Class
import SymTable
import TokenParser
import Eval
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
        e <- params
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
        (tk, tp) <- dataType
        b <- idToken
        return (getIdData b, tp)

refParam :: ParsecT [Token] MyState IO (String, Type)
refParam = do
        ref <- refToken
        (tk, tp) <- dataType
        b <- idToken
        return (getIdData b, tp)