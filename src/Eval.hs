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

cast (Lexer.CastingBool _) (Type.String value) = do
    let v = readMaybe value
    maybe (error ("Não é possível converter '" ++ value ++ "' para Bool")) Type.Bool v

cast (Lexer.CastingString _) (Type.Int value) = Type.String (show value)
cast (Lexer.CastingString _) (Type.Real value) = Type.String (show value)
cast (Lexer.CastingString _) (Type.Bool value) = Type.String (show value)
cast a b = error ("O cast entre valores '" ++ show a ++"' e '"++ show b ++ "' é inválido")


coercion :: Type -> Type -> Type
coercion (Type.Int _) (Type.Real value) = Type.Int (truncate value)
coercion (Type.Real _) (Type.Int value) = Type.Real (fromIntegral value)
coercion (Type.String _) (Type.Char value) = Type.String (show value)
coercion (Type.Int _) a = a
coercion (Type.Real _)  a = a
coercion (Type.String _)  a = a
coercion (Type.Char _)  a = a
coercion (Type.Bool _)  a = a
coercion Type.List {}  a = a
coercion Type.Struct {} a = a
coercion Type.Ref {} a = a

eval :: Type -> Token -> Type -> Type
eval (Type.String x) (Plus _ ) (Type.String y) = Type.String (x ++ y)
eval (Type.String x) (Plus _ ) (Type.Char y ) = Type.String (x ++ [y])
eval (Type.String x) (Plus _ ) (Type.Int y) = Type.String (x ++ show y)
eval (Type.String x) (Plus _ ) (Type.Real y) = Type.String (x ++ show y)
eval (Type.String x) (Plus _ ) (Type.Bool y) = Type.String (x ++ show y)
eval (Type.Char x) (Plus _ ) (Type.Char y ) = Type.String (x : [y])
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

getArgs :: Subprogram -> [(String, Type, Bool)]
getArgs (_, _, args, _) = args


isRefNull :: Type -> Bool
isRefNull (Type.Ref _ ref) = ref == ""
isRefNull _ = error "Operação de null aplicável apenas para referências"


-- Eval para acessar alguma posicao de uma array (1d)
            -- Type.List  rowIndex
evalArrayAcess :: Type -> Type  -> Type
evalArrayAcess (Type.List rows cols list) (Type.Int rowIndex)
    | cols > 0 = error "Tentativa de manipular uma Matriz como um Array"
    | rowIndex >= rows = error "Valor do indice maior que a capacidade do Array"
    | otherwise = accessArray rowIndex list
evalArrayAcess _ (Type.Int _) = error "Uso de subscrito em um tipo diferente de Array"
evalArrayAcess Type.List {} _ = error "Indice utilizado não é um inteiro"
evalArrayAcess _ _ = error "Tentativa de acesso de Array inadequado"


-- Eval para acessar alguma posicao de uma matriz (2d)
--                  list    row        col    
evalMatrixAcess :: Type -> Type ->  Type -> Type
evalMatrixAcess (Type.List numRows cols list)  (Type.Int rowIndex) (Type.Int columnIndex)
    | cols < 1 = error "Tentativa de manipular uma Matriz como um Array"
    | rowIndex >= numRows = error "Valor do indice das linhas maior que a quantidade de linhas da Matriz"
    | columnIndex >= cols = error "Valor do indice das colunas maior que a quantidade de colunas da Matriz"
    | otherwise = accessArray (columnIndex + rowIndex*numRows) list
evalMatrixAcess _ (Type.Int _) (Type.Int _) = error "Uso de dois subscritos em um tipo diferente de Matriz"
evalMatrixAcess Type.List {} _ (Type.Int _) = error "Indice utilizado para as linhas não é um inteiro"
evalMatrixAcess Type.List {} (Type.Int _) _ = error "Indice utilizado para as colunas não é um inteiro"
evalMatrixAcess _ _ _ = error "Tentativa de acesso de Matriz inadequado"


--Eval para criar array (1d)
--  't' é o valor que iremos utilizar para inicializar a array/matrix
                -- t       length
evalCreateArray :: Type -> Type  -> Type
evalCreateArray t (Type.Int l)
    | l == 0 = error "Não se pode declarar um array com quantidade nula de capacidade"
    | otherwise = Type.List l 0 (createArray l t)
evalCreateArray _ _ = error "Deu ruim na criação do array"


--Eval para criar matriz (array 2d)
--  't' é o valor que iremos utilizar para inicializar a array/matrix
            --       t      rows   cols    
evalCreateMatrix :: Type -> Type -> Type  -> Type
evalCreateMatrix t (Type.Int numRows) (Type.Int numCols)
    | numRows == 0 = error "Não se pode declarar uma matriz com quantidade nula de linhas"
    | numCols == 0 = error "Não se pode declarar uma matriz com quantidade nula de colunas"
    | otherwise = Type.List numRows numCols (createArray (numRows*numCols) t)
evalCreateMatrix _ _ _ = error "Deu ruim na criação da matriz"



            --         Type.List   rowIndex     newValue
evalArrayAssignment :: Type ->      Type ->      Type -> Type
evalArrayAssignment (Type.List rows cols list) (Type.Int rowIndex) newValue
    | cols > 0 = error "Tentativa de manipular uma Matriz como um Array"
    | rowIndex >= rows = error "Valor do indice maior que a capacidade do Array"
    | otherwise  = Type.List rows cols (assignValueArray rowIndex newValue list)
evalArrayAssignment _ (Type.Int _) _ = error "Uso de subscrito em um tipo diferente de Array"
evalArrayAssignment Type.List {} _ _ = error "Indice utilizado não é um inteiro"
evalArrayAssignment _ _ _ = error "Tentativa de acesso de Array inadequado"

            --         Type.List      rowIndex  colIndex    newValue
evalMatrixAssignment :: Type ->      Type ->   Type ->     Type     -> Type
evalMatrixAssignment (Type.List numRows numCols list) (Type.Int rowIndex) (Type.Int colIndex) newValue
    | numCols < 1 = error "Tentativa de manipular uma Matriz como um Array"
    | rowIndex >= numRows = error "Valor do indice das linhas maior que a quantidade de linhas da Matriz"
    | colIndex >= numCols = error "Valor do indice das colunas maior que a quantidade de colunas da Matriz"
    | otherwise = Type.List numRows numCols (assignValueArray (colIndex + rowIndex*numCols) newValue list)
evalMatrixAssignment _ (Type.Int _) (Type.Int _) _ = error "Uso de dois subscritos em um tipo diferente de Matriz"
evalMatrixAssignment Type.List {} _ (Type.Int _) _ = error "Indice utilizado para as linhas não é um inteiro"
evalMatrixAssignment Type.List {} (Type.Int _) _ _ = error "Indice utilizado para as colunas não é um inteiro"
evalMatrixAssignment _ _ _ _ = error "Tentativa de acesso de Matriz inadequado"




--Eval para acessar alguma posicao da lista (array 1d ou array 2d)
accessArray :: Int -> [Type] -> Type
accessArray _ [] = error "O indice maior do que a capacidade permitida"
accessArray index (x:xs)
                        | index > 0 = accessArray (index-1) xs
                        | index == 0 = x
                        | otherwise = error "Acesso fora dos limites" --Some error message

--Eval para criar um array (1d ou 2d)
createArray :: Int -> Type -> [Type]
createArray = replicate

--Eval para atribuir um valor a alguma posicao da array (1d ou 2d)
assignValueArray :: Int -> Type -> [Type] -> [Type]
assignValueArray _ _ [] = error "O indice maior do que a capacidade permitida"
assignValueArray index value (x:xs)
                                | index > 0 = x:assignValueArray (index-1) value xs
                                | index < 0 = error "Acesso fora dos limites" --Some error message
                                | otherwise = value:xs