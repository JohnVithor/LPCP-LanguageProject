module Eval where
import Lexer
import Type
import SymTable (Subprogram)
import Data.Maybe
import Text.Read

cast :: Token -> Type -> Type
cast (Lexer.CastingReal _) (Type.Int value) = Type.Real (fromIntegral value)
cast (Lexer.CastingReal _) (Type.Real value) = Type.Real value
cast (Lexer.CastingReal _) (Type.String value) = do
    let v = readMaybe value
    maybe (error ("Não é possível converter '" ++ value ++ "' para Real")) Type.Real v

cast (Lexer.CastingInt _) (Type.Int value) = Type.Int value
cast (Lexer.CastingInt _) (Type.Real value) = Type.Int (truncate value)
cast (Lexer.CastingInt _) (Type.String value) = do
    let v = readMaybe value
    maybe (error ("Não é possível converter '" ++ value ++ "' para Int")) Type.Int v

cast a b = error ("cast inválido: " ++ show a ++ show b)


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
eval (Type.Int x) (LessOrEqual _ ) (Type.Int y) = Type.Bool (x <= y)
eval (Type.Real x) (LessOrEqual _ ) (Type.Real y) = Type.Bool (x <= y)
eval (Type.Int x) (LessOrEqual _ ) (Type.Real y) = Type.Bool (fromIntegral x <= y)
eval (Type.Real x) (LessOrEqual _ ) (Type.Int y) = Type.Bool (x <= fromIntegral y)
eval (Type.Int x) (GreaterOrEqual _ ) (Type.Int y) = Type.Bool (x >= y)
eval (Type.Real x) (GreaterOrEqual _ ) (Type.Real y) = Type.Bool (x >= y)
eval (Type.Int x) (GreaterOrEqual _ ) (Type.Real y) = Type.Bool (fromIntegral x >= y)
eval (Type.Real x) (GreaterOrEqual _ ) (Type.Int y) = Type.Bool (x >= fromIntegral y)
eval (Type.Int x) (Equal _ ) (Type.Int y) = Type.Bool (x == y)
eval (Type.Real x) (Equal _ ) (Type.Real y) = Type.Bool (x == y)
eval (Type.Int x) (Equal _ ) (Type.Real y) = Type.Bool (fromIntegral x == y)
eval (Type.Real x) (Equal _ ) (Type.Int y) = Type.Bool (x == fromIntegral y)
eval (Type.String x) (Equal _ ) (Type.String y) = Type.Bool (x == y)
eval (Type.Char x) (Equal _ ) (Type.Char y) = Type.Bool (x == y)
eval (Type.Int x) (NotEqual _ ) (Type.Int y) = Type.Bool (x /= y)
eval (Type.Real x) (NotEqual _ ) (Type.Real y) = Type.Bool (x /= y)
eval (Type.Int x) (NotEqual _ ) (Type.Real y) = Type.Bool (fromIntegral x /= y)
eval (Type.Real x) (NotEqual _ ) (Type.Int y) = Type.Bool (x /= fromIntegral y)
eval (Type.Char x) (NotEqual _ ) (Type.Char y) = Type.Bool (x /= y)
eval (Type.String x) (NotEqual _ ) (Type.String y) = Type.Bool (x /= y)
eval (Type.Int x) (Less _ ) (Type.Int y) = Type.Bool (x < y)
eval (Type.Real x) (Less _ ) (Type.Real y) = Type.Bool (x < y)
eval (Type.Int x) (Less _ ) (Type.Real y) = Type.Bool (fromIntegral x < y)
eval (Type.Real x) (Less _ ) (Type.Int y) = Type.Bool (x < fromIntegral y)
eval (Type.Int x) (Greater _ ) (Type.Int y) = Type.Bool (x > y)
eval (Type.Real x) (Greater _ ) (Type.Real y) = Type.Bool (x > y)
eval (Type.Int x) (Greater _ ) (Type.Real y) = Type.Bool (fromIntegral x > y)
eval (Type.Real x) (Greater _ ) (Type.Int y) = Type.Bool (x > fromIntegral y)
eval _ _ _ = error "deu ruim"

evalUni :: Token -> Type -> Type
evalUni (Not _ ) (Type.Bool y) = Type.Bool (not y)
evalUni (Minus _ ) (Type.Int y) = Type.Int (-y)
evalUni (Minus _ ) (Type.Real y) = Type.Real (-y)
evalUni _ _ = error "deu ruim"

runFunc :: Subprogram -> [(String, Type)] -> Type
runFunc (name, t, args, stmts) params = fromJust t
