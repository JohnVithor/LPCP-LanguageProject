module SymTable where

import Lexer
import Type
import Data.List

type Subprogram = (String, Maybe Type, [(String, Type)], [Token])
type SymTable = (String,Type,Bool)
type MyState = ([SymTable], [Type], [Subprogram], Int, String)

getSymbolTbl:: MyState -> [SymTable]
getSymbolTbl (ty, _, _, _, _) = ty

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
enterScope (t, ty, programs,count,func) = (t, ty, programs,count+1,func)

exitScope :: MyState -> MyState
exitScope (t, ty, programs,count,func) = (t, ty, programs,count-1,func)

cleanVarsScope :: MyState -> MyState
cleanVarsScope (t, ty, programs,count,func) = (removeVarsInScope (func++"."++show count) t, ty, programs, count, func)

removeVarsInScope :: String -> [SymTable] -> [SymTable]
removeVarsInScope _ [] = []
removeVarsInScope prefix ((key, value, constFlag):t)
    | prefix `isPrefixOf` key = removeVarsInScope prefix t
    | otherwise = (key, value, constFlag):removeVarsInScope prefix t

callFunc :: String -> MyState -> MyState
callFunc name (t, ty, ref,count,_) = (t, ty, ref,count,name)

symtableGetValue :: String -> MyState -> SymTable
symtableGetValue name (t, _, _,count,func) = symtableGetInner (func,count,name) t t

symtableGetVar :: String -> MyState -> SymTable
symtableGetVar name (t, _, _,count,func) = symtableGetInner3 (func,count,name) t t

symtableGetInner :: (String,Int,String) -> [SymTable] -> [SymTable] -> SymTable
symtableGetInner (scope,count,name) ((key2, value, const1):t) backup
    | isRefType value = if key == key2 then symtableGetInner2 (getRefKey value) backup
                             else symtableGetInner (scope,count,name) t backup
    | otherwise = if key == key2 then (key2, value, const1)
                             else symtableGetInner (scope,count,name) t backup
    where key = scope++"."++show count++"."++name
symtableGetInner (scope,count,name) [] backup =
    if count > 0 then symtableGetInner (scope,count-1,name) backup backup
    else error ("Variável não encontrada: " ++ name)

symtableGetInner2 :: String  -> [SymTable] -> SymTable
symtableGetInner2 key ((key2, value, const1):t) =
    if key == key2 then (key2, value, const1)
    else symtableGetInner2 key t
symtableGetInner2 _ [] = error "Referencia não encontrada"

symtableGetInner3 :: (String,Int,String) -> [SymTable] -> [SymTable] -> SymTable
symtableGetInner3 (scope,count,name) ((key2, value, const1):t) backup
    = if key == key2 then (key2, value, const1)
                             else symtableGetInner3 (scope,count,name) t backup
    where key = scope++"."++show count++"."++name
symtableGetInner3 (scope,count,name) [] backup =
    if count > 0 then symtableGetInner3 (scope,count-1,name) backup backup
    else error ("Variável não encontrada: " ++ name)

symtableInsert :: SymTable -> MyState -> MyState
symtableInsert (name, value, const1) (symtable, t, subs,count, func) = symtableInsertInner (func++"."++show count++"."++name, value, const1) (symtable, t, subs,count, func)


symtableInsertInner :: SymTable -> MyState -> MyState
symtableInsertInner symbol ([], t, subs,count, func) = ([symbol], t, subs,count, func)
symtableInsertInner symbol (symtable, t, subs,count, func) = (symbol:symtable, t, subs,count, func)

symtableUpdate :: Bool -> Type -> SymTable -> MyState -> MyState
symtableUpdate x refVal tok (sym, ty, subs ,count, func) = (symtableUpdateInner x refVal tok sym, ty, subs,count,func)

symtableUpdateInner :: Bool -> Type -> SymTable -> [SymTable] -> [SymTable]
symtableUpdateInner _ _ (name, _, _) [] = error ("Variável não encontrada: " ++ name)
symtableUpdateInner x refVal (name, value, const1) ((name2, value2, const2):t)
    | const1 = error "Não se pode modificar uma constante"
    | x =   if getRefKey value == name2 then (name2, refVal, const2):t
            else (name2, value2, const2) : symtableUpdateInner x refVal (name, value, const1) t
    | otherwise =   if name == name2 then (name, value, const1):t
                    else (name2, value2, const2) : symtableUpdateInner x refVal (name, value, const1) t

symtableRemove :: SymTable -> [SymTable] -> [SymTable]
symtableRemove (name, _, _) [] = error ("Variável não encontrada: " ++ name)
symtableRemove (name, value, const1) ((name2, value2, const22):t) =
    if name == name2 then t
    else (name2, value2, const22) : symtableRemove (name, value, const1) t

getHeapId :: MyState -> Int 
getHeapId (ty, _, _, _, _) = getHeapIdInner 0 ty

getHeapIdInner :: Int -> [SymTable] -> Int
getHeapIdInner idt [] = idt
getHeapIdInner idt ((key, _, _):t)
    | "heap." `isPrefixOf` key = getHeapIdInner (idt+1) t
    | otherwise = getHeapIdInner idt t
