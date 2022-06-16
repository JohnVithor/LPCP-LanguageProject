module Eval where
import Lexer


eval :: Token -> Token -> Token -> Token
eval (String p x) (Plus _ ) (String _ y) = String p (x ++ y)
eval _ _ _ = error "deu ruim"