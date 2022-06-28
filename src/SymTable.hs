module SymTable where

import Lexer
import Type

type Subprogram = (String, Maybe Type, [(String, Type)], [Token])
type MyState = ([(String,Type)], [Type], [Subprogram])

getMainFunc :: MyState -> Subprogram
getMainFunc (_, _, subs) =  getMainFuncInner subs

getMainFuncInner :: [Subprogram] -> Subprogram
getMainFuncInner [] = error "main not found"
getMainFuncInner ((name, t, args, stmts):subs) =
                               if name == "main" then (name, t, args, stmts)
                               else getMainFuncInner subs

subsprogramTableInsert :: Subprogram -> MyState -> MyState
subsprogramTableInsert sub (ty, tbl, subs) = (ty, tbl, subs ++ [sub])

typeTableInsert :: Type -> MyState -> MyState
typeTableInsert t (ty, tbl, subs) = (ty, tbl ++ [t], subs)

typeTableGet :: Token -> MyState -> Type
typeTableGet (Type _ name) (_, tbl, _)
    | name == "bool" = Type.Bool False
    | name == "int" = Type.Int 0
    | name == "real" = Type.Real 0.0
    | name == "char" = Type.Char ' '
    | name == "string" = Type.String ""
    | otherwise = error "tipo primitivo não reconhecido"
typeTableGet (Id _ name) (_, tbl, _) = getUserDefinedType name tbl
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

symtableGet ::  String -> MyState -> Type
symtableGet name (sym, _, _) = symtableGetInner name sym

symtableGetInner :: String -> [(String,Type)] -> Type
symtableGetInner _ [] = error "variable not found"
symtableGetInner name ((id, value):t) =
                               if name == id then value
                               else symtableGetInner name t

symtableInsert :: (String,Type) -> MyState -> MyState
symtableInsert symbol ([], t, subs)  = ([symbol], t, subs)
symtableInsert symbol (symtable, t, subs) = (symtable ++ [symbol], t, subs)

symtableUpdate :: (String,Type) -> MyState -> MyState
symtableUpdate tok (sym, ty, subs) = (symtableUpdateInner tok sym, ty, subs)

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
