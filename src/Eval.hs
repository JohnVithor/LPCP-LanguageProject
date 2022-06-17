module Eval where
import Lexer
import Type

eval :: Type -> Token -> Type -> Type
eval (Type.String x) (Plus _ ) (Type.String y) = Type.String (x ++ y)
eval _ _ _ = error "deu ruim"