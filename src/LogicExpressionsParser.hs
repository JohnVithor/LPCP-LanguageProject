
import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Eval (eval)
import TokenParser

--a or b
orExp :: ParsecT [Token] [(Token,Token)] IO [Token]
orExp = do
            a <- logicExp
            b <- orToken
            c <- logicExp
            return (a:b:[c])

--a and b
andExp :: ParsecT [Token] [(Token,Token)] IO [Token]
andExp = do
            a <- logicExp
            b <- andToken
            c <- logicExp
            return (a:b:[c])

--not a
notExp :: ParsecT [Token] [(Token,Token)] IO [Token]
notExp = do
            a <- notToken
            b <- logicExp
            return (a:[b])

-- a==b <|> a!=b <|> a<b <|> a>b <|> a>=b <|> a<=b
comparisson :: ParsecT [Token] [(Token,Token)] IO [Token]
comparisson = do
            a <- expression
            b <- equalToken <|> notEqualToken <|> lessToken <|> greaterToken <|> greaterOrEqualToken <|> lessOrEqualToken
            c <- expression
            return (a:b:[c])

parentesisLogicExp :: ParsecT [Token] [(Token,Token)] IO [Token]
parentesisLogicExp = do
            a <- BeginExpression
            b <- inToken
            c <- EndExpression
            return (a:b:[c])

--a in lista
inclusion :: ParsecT [Token] [(Token,Token)] IO [Token]
inclusion = do
            a <- expression
            b <- inToken
            c <- idToken
            return (a:b:[c])

-- b is int
typeCheck :: ParsecT [Token] [(Token,Token)] IO [Token]
typeCheck = do
            a <- expression
            b <- isToken
            c <- typeToken
            return (a:b:[c])

logicExp :: ParsecT [Token] [(Token,Token)] IO [Token]
logicExp = do
            a <- (boolToken <|> idToken <|> notToken <|> andToken <|> orToken <|> ComparissonToken <|> parentesisLogicExp <|> inclusion <|> typeCheck)
            return (a)