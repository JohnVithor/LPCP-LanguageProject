module Eval where
import Lexer
import Type


eval :: Type -> Token -> Type -> Type
eval (Type.String x) (Plus _ ) (Type.String y) = Type.String (x ++ y)
eval (Type.Int x) (Plus _ ) (Type.Int y _) = Type.Int (x + y) 
eval (Type.Real x) (Plus _ ) (Type.Real y _) = Type.Real (x + y) 
eval (Type.Int x) (Plus _ ) (Type.Real y _) = Type.Real (x + y) 
eval (Type.Real x) (Plus _ ) (Type.Int y _) = Type.Real (x + y) 
eval (Type.Char x) (Plus _ ) (Type.Char y _) = Type.String ([x] ++ [y]) 
eval (Type.String x) (Plus _ ) (Type.Char y _) = Type.String (x ++ [y]) 
eval (Type.Char x) (Plus _ ) (Type.String y _) = Type.String ([x] ++ y) 

eval _ _ _ = error "deu ruim"