module Eval where
import Lexer
import Type
import Text.Read
import SymTable

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
eval t1 op t2 = error ("A operação " ++ show op ++" não está definida para os tipos " ++ show t1 ++" e " ++ show t2)


evalUni :: Token -> Type -> Type
evalUni (Not _ ) (Type.Bool y) = Type.Bool (not y)
evalUni (Minus _ ) (Type.Int y) = Type.Int (-y)
evalUni (Minus _ ) (Type.Real y) = Type.Real (-y)
evalUni op t = error ("A operação " ++ show op ++" não está definida para o tipo " ++ show t)

getStmts :: Subprogram -> [Token]
getStmts (_, _, _, stmts) = stmts

getArgs :: Subprogram -> [(String, Type)]
getArgs (_, _, args, _) = args


evalArray :: [Type] -> Token -> Type -> Token -> Type
evalArray ([Type.Int list]) (BeginListConst _) (Type.Int index) (EndListConst _) = Type.Int (accessArray index list)
evalArray ([Type.Real list]) (BeginListConst _) (Type.Int index) (EndListConst _) = Type.Real (accessArray index list)
evalArray ([Type.Bool list]) (BeginListConst _) (Type.Int index) (EndListConst _) = Type.Bool (accessArray index list)
evalArray ([Type.String list]) (BeginListConst _) (Type.Int index) (EndListConst _) = Type.String (accessArray index list)
evalArray ([Type.Char list]) (BeginListConst _) (Type.Int index) (EndListConst _) = Type.Char (accessArray index list)
evalArray list tk1 type tk2 = error ("O valor de " ++ type ++" não é inteiro")

accessArray :: Int -> [Type] -> Type
accessArray index (x:xs) 
                        | index > 0 = (accessArray (index-1) xs)
                        | index == 0 = x
                        | otherwise = error ("Access out of bounds!") --Some error message

createArrayInt :: Int -> [Int]
createArrayInt length = replicate length 0

createArrayReal :: Int -> [Float]
createArrayReal length = replicate length 0.0

createArrayString :: Int -> [Char]
createArrayString length = replicate length ""

createArrayBool :: Int -> [Bool]
createArrayBool length = replicate length False


assignValueArray :: Int -> Type -> [Type] -> [Type]
assignValueArray index value (x:xs)
                                | index > 0 = x:(assignValue (index-1) value xs)
                                | index < 0 = error ("Access out of bounds!") --Some error message
                                | otherwise = value:xs