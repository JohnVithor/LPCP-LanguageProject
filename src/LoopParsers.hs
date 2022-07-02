module LoopParsers where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Eval (eval)
import TokenParser
import SymTable
import Control.Monad
import Type


-- begin repeat : stmts until (logicExpression) end repeat
repeatUntilLoop :: ParsecT [Token] u IO [Token]
repeatUntilLoop = do
                  a <- beginScopeToken
                  b <- repeatToken
                  c <- colonToken
                  d <- stmts
                  e <- untilToken
                  f <- openParenthesesToken
                  g <- logicExpressionToken       --CHANGE
                  h <- closeParenthesesToken
                  i <- endScopeToken
                  j <- repeatToken
                  if exemode the
                    if eval (g op a)
                        eval d
                    return (a:b:c:d:e:f:g:h:i:[j])
                  else 
                    return (a:b:c:d:e:f:g:h:i:[j])





-- begin foreach <data_type> var_id in var_id: <statements> end foreach
foreachLoop :: ParsecT [Token] u IO Type
foreachLoop = do
              a <- beginScopeToken
              b <- foreachToken
              c <- typeToken       -- CHANGE
              d <- idToken
              e <- inToken
              f <- colonToken
              g <- stmts          -- NOT IMPLEMENTED YET
              h <- endScopeToken
              i <- foreachToken
              return (a:b:c:d:e:f:g:h:[i])
