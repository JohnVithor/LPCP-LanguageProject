module Main where

import Parser ( parser )
import Lexer ( getTokens )
import System.IO.Unsafe
import System.Environment
import Data.String


main = do
  let arg = getArgs
  case unsafePerformIO (parser (getTokens (fromString (head (unsafePerformIO arg))))) of
            { Left err -> print err;
              Right ans -> print ans
            }
