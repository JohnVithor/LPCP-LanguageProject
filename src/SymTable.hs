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
typeTableGet (Type _ name) (_, _, _)
    | name == "bool" = Type.Bool False
    | name == "int" = Type.Int 0
    | name == "real" = Type.Real 0.0
    | name == "char" = Type.Char ' '
    | name == "string" = Type.String ""
    | otherwise = error "tipo primitivo não reconhecido"
typeTableGet (Id _ name) (_, tbl, _) = getUserDefinedType name tbl
typeTableGet _ _ = error "Not a type token"

getLogExprResult :: Type -> Bool
getLogExprResult (Type.Bool v) = v
getLogExprResult _ = error "Not a Boolean value"


getUserDefinedType :: String -> [Type] -> Type
getUserDefinedType _ [] = error "Type not Found"
getUserDefinedType name (t:tbl)
    | name == getStructName t = t
    | otherwise = getUserDefinedType name tbl

getDefaultValue :: Token -> Type
getDefaultValue (Type _ "int") = Type.Int 0
getDefaultValue (Type _ "real") = Type.Real 0.0
getDefaultValue (Type _ "char") = Type.Char ' '
getDefaultValue (Type _ "string") = Type.String ""
getDefaultValue (Type _ "bool") = Type.Bool True
-- Todo incluir busca nos tipos de usuarios (structs) ?
getDefaultValue _ = error "Is not a Type Token"

symtableGet :: String -> MyState -> Maybe Type
symtableGet name ((name2, value):t, a, b) = if name == name2 then Just value
                             else symtableGet name (t, a, b)
symtableGet _ ([], _, _) = fail "variable not found"

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
