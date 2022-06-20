module Type where

data Type = Bool Bool       | 
            Int Int         |
            Real Double     | 
            Char Char       | 
            String String   |
            List [Type]     |
            Struct String [(String, Type)]
            deriving (Eq,Show)


getStructName :: Type -> String
getStructName (Struct name _) = name
getStructName _ = error "Not a Struct"
