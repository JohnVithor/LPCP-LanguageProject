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



-- Eval para acessar alguma posicao de uma array (1d)
            -- Type.List  rowIndex
evalArrayAcess :: Type -> Type  -> Type
evalArrayAcess (Type.List numRows _ list) (Type.Int rowIndex) = accessArray rowIndex list
evalArrayAcess list type_index = error ("Indice não é um inteiro")



-- Eval para acessar alguma posicao de uma matriz (2d)
--                  list    row        col    
eval2dArrayAcess :: Type -> Type ->  Type -> Type
eval2dArrayAcess (Type.List numRows _ list)  (Type.Int rowIndex)   (Type.Int columnIndex) = accessArray (columnIndex + rowIndex*numRows) list
eval2dArrayAcess type index_row index_col  = error ("Indice não é um inteiro")





--Eval para criar array (1d)
-- 't' sera o valor atribuido na inicializacao
evalCreateArray :: Type -> Type  -> Type
evalCreateArray t (Type.Int length) = Type.List length 1 (createArray length t)


--Eval para criar matriz (array 2d)
evalCreateMatrix :: Type -> Type -> Type  -> Type
evalCreateMatrix t (Type.Int numRows) (Type.Int numCols) = Type.List numRows numCols (createArray numRows*numCols t)



--Eval para acessar alguma posicao da lista (array 1d ou array 2d)
accessArray :: Int -> [Type] -> Type
accessArray index (x:xs) 
                        | index > 0 = (accessArray (index-1) xs)
                        | index == 0 = x
                        | otherwise = error ("Access out of bounds!") --Some error message



--Eval para criar um array (1d ou 2d)
createArray :: Int -> Type -> [Type]
createArray length v = replicate length v



            --         Type.List   rowIndex     newValue
evalArrayAssignment :: Type ->      Type ->      Type -> [Type]
evalArrayAssignment (Type.List numRows _ list) (Type.Int rowIndex) (Type.Int newValue) = assignValueArray rowIndex newValue list
evalArrayAssignment list type_index new_value = error ("Indice não é um inteiro")

            --         Type.List      rowIndex  colIndex    newValue
eval2dArrayAssignment :: Type ->      Type ->   Type ->     Type     -> [Type]
evalArrayAssignment (Type.List numRows numCols list) (Type.Int rowIndex) (Type.Int colIndex) (Type.Int newValue) = assignValueArray (columnIndex + rowIndex*numRows) newValue list
evalArrayAssignment type index_row index_col new_value  = error ("Indice não é um inteiro")


--Eval para atribuir um valor a alguma posicao da array (1d ou 2d)
assignValueArray :: Int -> Type -> [Type] -> [Type]
assignValueArray index value (x:xs)
                                | index > 0 = x:(assignValue (index-1) value xs)
                                | index < 0 = error ("Access out of bounds!") --Some error message
                                | otherwise = value:xs