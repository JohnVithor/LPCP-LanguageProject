module SymTable where

import Lexer
import Type

typeTableInsert :: Type -> ([(String,Type)], [Type]) -> ([(String,Type)], [Type])
typeTableInsert t (ty, tbl) = (ty, tbl ++ [t])

typeTableGet :: Token -> ([(String,Type)], [Type]) -> Type
typeTableGet (Type _ name) (_, tbl)
    | name == "bool" = Type.Bool False
    | name == "int" = Type.Int 0
    | name == "real" = Type.Real 0.0
    | name == "char" = Type.Char ' '
    | name == "string" = Type.String ""
    | otherwise = error "tipo primitivo não reconhecido"
typeTableGet (Id _ name) (_, tbl) = getUserDefinedType name tbl
typeTableGet _ _ = error "Not a type token"

getUserDefinedType :: String -> [Type] -> Type
getUserDefinedType name [] = error "Type not Found"
getUserDefinedType name (t:tbl)
    | name == getStructName t = t
    | otherwise = getUserDefinedType name tbl

getDefaultValue :: Token -> Type
getDefaultValue (Type p "int") = Type.Int 0
getDefaultValue (Type p "real") = Type.Real 0.0
getDefaultValue (Type p "char") = Type.Char ' '
getDefaultValue (Type p "string") = Type.String ""
getDefaultValue (Type p "bool") = Type.Bool True
-- Todo incluir busca nos tipos de usuarios (structs) ?
getDefaultValue _ = error "Is not a Type Token"

symtableGet ::  String -> ([(String,Type)], [Type]) -> Type
symtableGet name (sym, ty) = symtableGetInner name sym

symtableGetInner :: String -> [(String,Type)] -> Type
symtableGetInner _ [] = error "variable not found"
symtableGetInner name ((id, value):t) =
                               if name == id then value
                               else symtableGetInner name t

symtableInsert :: (String,Type) -> ([(String,Type)], [Type]) -> ([(String,Type)], [Type])
symtableInsert symbol ([], t)  = ([symbol], t)
symtableInsert symbol (symtable, t) = (symtable ++ [symbol], t)

symtableUpdate :: (String,Type) -> ([(String,Type)], [Type]) -> ([(String,Type)], [Type])
symtableUpdate tok (sym, ty) = (symtableUpdateInner tok sym, ty)

symtableUpdateInner :: (String,Type) -> [(String,Type)] -> [(String,Type)]
symtableUpdateInner _ [] = fail "variable not found"
symtableUpdateInner (id1, v1) ((id2, v2):t) =
                               if id1 == id2 then (id1, v1) : t
                               else (id2, v2) : symtableUpdateInner (id1, v1) t

symtableRemove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtableRemove _ [] = fail "variable not found"
symtableRemove (id1, v1) ((id2, v2):t) =
                               if id1 == id2 then t
                               else (id2, v2) : symtableRemove (id1, v1) t
