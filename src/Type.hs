module Type where

data Type = Bool Bool                       | 
            Int Int                         |
            Real Double                     | 
            Char Char                       | 
            String String                   |
            List Int Int [Type]             |
            Struct String [(String, Type)]  |
            Ref String String               
            deriving (Eq,Show)

getTypeName :: Type -> String
getTypeName (Type.Bool _) = "bool"
getTypeName (Type.Int _) = "int"
getTypeName (Type.Real _) = "real"
getTypeName (Type.Char _) = "char"
getTypeName (Type.String _) = "string"
getTypeName Type.List {} = "list"
getTypeName (Type.Struct name _) = name
getTypeName (Type.Ref _ _) = "ref"

getStructName :: Type -> String
getStructName (Type.Struct name _) = name
getStructName _ = error "Not a Struct"

getStructField :: Type -> String -> Type
getStructField (Type.Struct _ values) field = getStructFieldInner values field
-- getStructField (Type.Ref (Type.Struct _ values) _) field = getStructFieldInner values field
getStructField t field = error ("Não é possivel acessar o campo '"++field++"' no tipo: " ++show t)

printVal :: Type -> IO()
printVal (Type.Bool v) = putStr (show v)
printVal (Type.Int v) = putStr (show v)
printVal (Type.Real v) = putStr (show v)
printVal (Type.Char v) = putChar v
printVal (Type.String v) = putStr v
printVal (Type.List _ _ v) = putStr (show v)
printVal (Type.Struct name _) =  putStr name
printVal (Type.Ref v ref) = putStr (v++"["++ref++"]")

printVals ::[(String, Type)] -> IO()
printVals ((_,v):vs) = do
    putStr ","
    printVal v
    printVals vs
printVals [] = putStr ""


getStructFieldInner :: [(String, Type)] -> String -> Type
getStructFieldInner [] field = error ("campo " ++ field++ " não encontrado")
getStructFieldInner ((name, value):values) field 
    | field == name = value
    | otherwise = getStructFieldInner values field


compatible :: Type -> Type -> Bool
compatible (Type.Int _) (Type.Int _) = True
compatible (Type.Int _) (Type.Real _) = True
compatible (Type.Real _) (Type.Real _) = True
compatible (Type.Real _) (Type.Int _) = True
compatible (Type.String _) (Type.String _) = True
compatible (Type.String _) (Type.Char _) = True
compatible (Type.Char _) (Type.Char _) = True
compatible (Type.Bool _) (Type.Bool _) = True
compatible (Type.Struct name1 _) (Type.Struct name2 _) = name1 == name2
compatible (Type.Ref x1 _) (Type.Ref x2 _) = x1 == x2
compatible (Type.List rows1 cols1 _) (Type.List rows2 cols2 _) = rows1 == rows2 && cols1 == cols2
compatible _ _ = False

isRefOf :: Type -> Type -> Bool 
isRefOf (Type.Ref x _) t = x == getTypeName t
isRefOf _ _ = False

isRefType :: Type -> Bool 
isRefType (Type.Ref _ _) = True
isRefType _ = False

getRefKey :: Type -> String
getRefKey (Type.Ref _ key) = key
getRefKey _ = ""

-- setRefValue :: Type -> Type -> Type
-- setRefValue (Type.Ref _ n) v = Type.Ref v n
-- setRefValue a _ = error ("Não é do Tipo Ref!: " ++ show a)

-- getRefValue :: Type -> Type
-- getRefValue (Type.Ref v _) = v
-- getRefValue a = error ("Não é do Tipo Ref!: " ++ show a)