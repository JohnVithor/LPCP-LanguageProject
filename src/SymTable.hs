module SymTable where

import Lexer
import Type

type Subprogram = (String, Maybe Type, [(String, Type)], [Token])
type MyState = ([(String,Type,Bool,Bool,String)], [Type], [Subprogram], Int, String)

getMainFunc :: MyState -> Subprogram
getMainFunc (_, _, subs, _, _) =  getMainFuncInner subs

getMainFuncInner :: [Subprogram] -> Subprogram
getMainFuncInner [] = error "main not found"
getMainFuncInner ((name, t, args, stmts):subs) =
                               if name == "main" then (name, t, args, stmts)
                               else getMainFuncInner subs

subsprogramTableInsert :: Subprogram -> MyState -> MyState
subsprogramTableInsert sub (ty, tbl, subs, count, func) = (ty, tbl, subs ++ [sub],count, func)

typeTableInsert :: Type -> MyState -> MyState
typeTableInsert t (ty, tbl, subs,count,func) = (ty, tbl ++ [t], subs,count,func)

typeTableGet :: Token -> MyState -> Type
typeTableGet (Type _ name) (_, _, _,_,_)
    | name == "bool" = Type.Bool False
    | name == "int" = Type.Int 0
    | name == "real" = Type.Real 0.0
    | name == "char" = Type.Char ' '
    | name == "string" = Type.String ""
    | otherwise = error "tipo primitivo não reconhecido"
typeTableGet (Id _ name) (_, tbl, _,_,_) = getUserDefinedType name tbl
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

enterScope :: MyState -> MyState
enterScope (t, ty, ref,count,func) = (t, ty, ref,count+1,func)

exitScope :: MyState -> MyState
exitScope (t, ty, ref,count,func) = (t, ty, ref,count-1,func)

callFunc :: String -> MyState -> MyState
callFunc name (t, ty, ref,count,_) = (t, ty, ref,count,name)

symtableGet :: String -> MyState -> (String,Type,Bool,Bool,String)
symtableGet name (t, _, _,count,func) = symtableGetInner (func,count,name) t t

symtableGetInner :: (String,Int,String) -> [(String,Type,Bool,Bool,String)] -> [(String,Type,Bool,Bool,String)] -> (String,Type,Bool,Bool,String)
symtableGetInner (scope,count,name) ((key2, value, flag, const1, ref):t) backup
    | flag = if key == key2 then symtableGetInner2 ref backup
                             else symtableGetInner (scope,count,name) t backup
    | otherwise = if key == key2 then (key2, value, flag, const1, ref)
                             else symtableGetInner (scope,count,name) t backup
    where key = scope++"."++show count++"."++name
symtableGetInner (scope,count,name) [] backup = 
    if count > 0 then symtableGetInner (scope,count-1,name) backup backup
    else error ("Variável não encontrada: " ++ name)

symtableGetInner2 :: String  -> [(String,Type,Bool,Bool,String)] -> (String,Type,Bool,Bool,String)
symtableGetInner2 key ((key2, value, flag, const1,ref):t) =
    if key == key2 then (key2, value, flag, const1,ref)
    else symtableGetInner2 key t
symtableGetInner2 _ [] = error "Referencia não encontrada"


symtableInsert :: (String,Type,Bool,Bool,String) -> MyState -> MyState
symtableInsert (name, value, ref, const1, othername) (symtable, t, subs,count, func) = symtableInsertInner (func++"."++show count++"."++name, value, ref, const1, othername) (symtable, t, subs,count, func)


symtableInsertInner :: (String,Type,Bool,Bool,String) -> MyState -> MyState
symtableInsertInner symbol ([], t, subs,count, func) = ([symbol], t, subs,count, func)
symtableInsertInner symbol (symtable, t, subs,count, func) = (symbol:symtable, t, subs,count, func)

symtableUpdate :: (String,Type,Bool,Bool,String) -> MyState -> MyState
symtableUpdate tok (sym, ty, subs ,count, func) = (symtableUpdateInner tok sym, ty, subs,count,func)

symtableUpdateInner :: (String,Type,Bool,Bool,String) -> [(String,Type,Bool,Bool,String)] -> [(String,Type,Bool,Bool,String)]
symtableUpdateInner _ [] = fail "variable not found"
symtableUpdateInner (name, value, ref, const1, othername) ((name2, value2, ref2, const2, othername2):t)
    | const1 = fail "Não se pode modificar uma constante"
    | ref =         if othername == name2 then (name2, value, ref2, const2, othername2):t
                    else (name2, value2, ref2, const2,othername2) : symtableUpdateInner (name, value, ref, const1, othername) t
    | otherwise =   if name == name2 then (name, value, ref, const1, othername):t
                    else (name2, value2, ref2, const2,othername2) : symtableUpdateInner (name, value, ref, const1, othername) t

symtableRemove :: (String,Type,Bool,Bool,String) -> [(String,Type,Bool,Bool,String)] -> [(String,Type,Bool,Bool,String)]
symtableRemove _ [] = fail "variable not found"
symtableRemove (name, value, ref, const1, othername) ((name2, value2, ref2, const2, othername2):t) =
    if name == name2 then t
    else (name2, value2, ref2, const2, othername2) : symtableRemove (name, value, ref, const1, othername) t
