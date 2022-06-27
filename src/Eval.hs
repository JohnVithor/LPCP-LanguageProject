module Eval where
import Lexer
import Type


eval :: Type -> Token -> Type -> Type
eval (Type.String x) (Plus _ ) (Type.String y) = Type.String (x ++ y)
eval (Type.Char x) (Plus _ ) (Type.Char y ) = Type.String (x : [y])
eval (Type.String x) (Plus _ ) (Type.Char y ) = Type.String (x ++ [y])
eval (Type.Char x) (Plus _ ) (Type.String y ) = Type.String (x : y)
eval (Type.Int x) (Plus _ ) (Type.Int y) = Type.Int (x + y)
eval (Type.Real x) (Plus _ ) (Type.Real y) = Type.Real (x + y)
eval (Type.Int x) (Plus _ ) (Type.Real y) = Type.Real (fromIntegral x + y)
eval (Type.Real x) (Plus _ ) (Type.Int y) = Type.Real (x + fromIntegral y)
eval (Type.Int x) (Minus _ ) (Type.Int y) = Type.Int (x - y)
eval (Type.Real x) (Minus _ ) (Type.Real y) = Type.Real (x - y)
eval (Type.Int x) (Minus _ ) (Type.Real y) = Type.Real (fromIntegral x - y)
eval (Type.Real x) (Minus _ ) (Type.Int y) = Type.Real (x - fromIntegral y)
eval (Type.Int x) (Mult _ ) (Type.Int y) = Type.Int (x * y)
eval (Type.Real x) (Mult _ ) (Type.Real y) = Type.Real (x * y)
eval (Type.Int x) (Mult _ ) (Type.Real y) = Type.Real (fromIntegral x * y)
eval (Type.Real x) (Mult _ ) (Type.Int y) = Type.Real (x * fromIntegral y)
eval (Type.Int x) (Div _ ) (Type.Int y) = Type.Int (x `div` y)
eval (Type.Real x) (Div _ ) (Type.Real y) = Type.Real (x / y)
eval (Type.Int x) (Div _ ) (Type.Real y) = Type.Real (fromIntegral x / y)
eval (Type.Real x) (Div _ ) (Type.Int y) = Type.Real (x / fromIntegral y)
eval (Type.Int x) (Mod _ ) (Type.Int y) = Type.Int (x `mod` y)
eval (Type.Bool x) (And _ ) (Type.Bool y) = Type.Bool (x && y)
eval (Type.Bool x) (Or _ ) (Type.Bool y) = Type.Bool (x || y)
eval _ _ _ = error "deu ruim"

evalUni :: Token -> Type -> Type
evalUni (Not _ ) (Type.Bool y) = Type.Bool (not y)
evalUni _ _ = error "deu ruim"
