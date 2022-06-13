module Main where 

import Parser ( parser )
import Lexer ( getTokens )
import System.IO.Unsafe


main :: IO ()
main = case unsafePerformIO (parser (getTokens "programaV1V2.pe")) of
            { Left err -> print err;
              Right ans -> print ans
            }
