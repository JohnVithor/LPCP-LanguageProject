module Main (main) where

import Text.Parsec
import Lexer
import TerminalParser

-- parsers para os não-terminais TODO:

program :: Parsec [Token] st [Token]
program = do
            a <- programToken
            b <- idToken
            c <- beginToken
            d <- stmts
            e <- endToken
            eof
            return (a:b:[c] ++ d ++ [e])

stmts :: Parsec [Token] st [Token]
stmts = do
          first <- assign
          next <- remainingStmts
          return (first ++ next)

assign :: Parsec [Token] st [Token]
assign = do
          a <- idToken
          b <- assignToken
          c <- intToken
          return (a:b:[c])

remainingStmts :: Parsec [Token] st [Token]
remainingStmts = (do a <- semiColonToken
                     b <- assign
                     return (a:b)) <|> return []

-- invocação do parser para o símbolo de partida 

parser :: [Token] -> Either ParseError [Token]
parser = runParser program () "Error message"

main :: IO ()
main = case parser (getTokens "programaV0.pe") of
            { Left err -> print err;
              Right ans -> print ans
            }