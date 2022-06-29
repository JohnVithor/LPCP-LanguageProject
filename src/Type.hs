module Type where

import Lexer

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

compatible :: Type -> Type -> Bool
compatible (Type.Int _) (Type.Int _) = True
compatible (Type.Real _) (Type.Real _) = True
compatible (Type.String _) (Type.String _) = True
compatible (Type.Char _) (Type.Char _) = True
compatible (Type.Bool _) (Type.Bool _) = True
compatible _ _ = False