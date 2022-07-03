module Type where

data Type = Bool Bool        | 
            Int Int          |
            Real Double      | 
            Char Char        | 
            String String    |
            List Type [Type] |
            Struct String [(String, Type)]
            deriving (Eq,Show)


getStructName :: Type -> String
getStructName (Type.Struct name _) = name
getStructName _ = error "Not a Struct"

getStructField :: Type -> String -> Type
getStructField (Type.Struct _ values) field = getStructFieldInner values field
getStructField _ _ = error "Not a Struct"

printVal :: Type -> IO()
printVal (Type.Bool v) = putStr (show v)
printVal (Type.Int v) = putStr (show v)
printVal (Type.Real v) = putStr (show v)
printVal (Type.Char v) = putStr (show v)
printVal (Type.String v) = putStr (show v)
printVal (Type.List _ v) = putStr (show v)
printVal (Type.Struct name []) = do 
                putStr (name ++ "()")
printVal (Type.Struct name ((_,v):vs)) = do 
                putStr (name ++ "(")
                printVal v
                printVals vs
                putStr ")"
                
printVals ::[(String, Type)] -> IO()
printVals ((_,v):vs) = do
    putStr ","
    printVal v
    printVals vs
printVals [] = putStr ""


getStructFieldInner :: [(String, Type)] -> String -> Type
getStructFieldInner [] _ = error "deu ruim"
getStructFieldInner ((name, value):values) field 
    | field == name = value
    | otherwise = getStructFieldInner values field


compatible :: Type -> Type -> Bool
compatible (Type.Int _) (Type.Int _) = True
compatible (Type.Real _) (Type.Real _) = True
compatible (Type.String _) (Type.String _) = True
compatible (Type.Char _) (Type.Char _) = True
compatible (Type.Bool _) (Type.Bool _) = True
compatible (Type.Struct name1 _) (Type.Struct name2 _) = name1 == name2
compatible _ _ = False