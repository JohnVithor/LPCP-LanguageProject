module Main where 

import Parser ( parser )
import Lexer ( getTokens )
import System.IO.Unsafe


main :: IO ()
main = case unsafePerformIO (parser (getTokens "exemploConst.LEEBA")) of
            { Left err -> print err;
              Right ans -> print ans
            }
