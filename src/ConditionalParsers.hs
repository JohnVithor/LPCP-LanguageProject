module ConditionalParsers where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Eval (eval)
import TokenParser
import SymTable
import Control.Monad
import Type


-- <conditional> := begin if ( <logic_expression> ): <statements> end if
ifConditional :: ParsecT [Token] u IO Type
ifConditional = do
                a <- beginScopeToken
                b <- ifToken
                c <- openParenthesesToken
                d <- logicExpressionToken
                e <- closeParenthesesToken
                f <- colonToken
                g <- stmts          -- NOT IMPLEMENTED YET
                h <- endScopeToken
                i <- ifToken
                return (a:b:c:d:e:f:g:h:[i])

--begin if ( <logic_expression> ): <statements> end if ; begin else: <statements> end else
ifElseConditional :: ParsecT [Token] u IO Type
ifElseConditional = do
                    a <- ifConditional
                    b <- semiColonToken
                    c <- beginScopeToken
                    d <- elseToken
                    e <- colonToken
                    f <- stmts          -- NOT IMPLEMENTED YET
                    g <- endScopeToken
                    h <- elseToken
                    return (a:b:c:d:e:f:g:[h])
