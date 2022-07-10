module Type where

data Type = Bool Bool                       | 
            Int Int                         |
            Real Double                     | 
            Char Char                       | 
            String String                   |
            List Type [Type]                |
            Struct String [(String, Type)]  |
            Ref String String               
            deriving (Eq,Show)

getTypeName :: Type -> String
getTypeName (Type.Bool _) = "Bool"
getTypeName (Type.Int _) = "Int"
getTypeName (Type.Real _) = "Real"
getTypeName (Type.Char _) = "Char"
getTypeName (Type.String _) = "String"
getTypeName (Type.List _ _) = "List"
getTypeName (Type.Struct name _) = name
getTypeName (Type.Ref _ _) = "Ref"

getStructName :: Type -> String
getStructName (Type.Struct name _) = name
getStructName _ = error "Not a Struct"

getStructField :: Type -> String -> Type
getStructField (Type.Struct _ values) field = getStructFieldInner values field
-- getStructField (Type.Ref (Type.Struct _ values) _) field = getStructFieldInner values field
getStructField _ _ = error "Not a Struct"

printVal :: Type -> IO()
printVal (Type.Bool v) = putStr (show v)
printVal (Type.Int v) = putStr (show v)
printVal (Type.Real v) = putStr (show v)
printVal (Type.Char v) = putStr (show v)
printVal (Type.String v) = putStr (show v)
printVal (Type.List _ v) = putStr (show v)
printVal (Type.Struct name _) =  putStr name
printVal (Type.Ref v ref) = putStr (ref++"["++show v++"]")

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
compatible (Type.Ref x1 _) (Type.Ref x2 _) = x1 == x2
compatible _ _ = False

ifRefOf :: Type -> Type -> Bool 
ifRefOf (Type.Ref x _) t = x == getTypeName t
ifRefOf _ _ = False

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