module SymTable where

import Lexer
import Type
import Data.List

type Subprogram = (String, Maybe Type, [(String, Type)], [Token])
type SymTable = (String,Type,Bool)
type MyState = ([SymTable], [Type], [Subprogram], Int, String,Int)

getSymbolTbl:: MyState -> [SymTable]
getSymbolTbl (ty, _, _, _, _, _) = ty

getMainFunc :: MyState -> Subprogram
getMainFunc (_, _, subs, _, _, _) =  getSubProgInner subs "main"

getSubProg :: String -> MyState -> Subprogram
getSubProg key (_, _, subs, _, _, _) =  getSubProgInner subs key

getSubProgInner :: [Subprogram] -> String -> Subprogram
getSubProgInner [] key = error ("O subprograma: '"++key++"' não foi encontrado")
getSubProgInner ((name, t, args, stmts):subs) key =
                               if name == key then (name, t, args, stmts)
                               else getSubProgInner subs key

subsprogramTableInsert :: Subprogram -> MyState -> MyState
subsprogramTableInsert sub (ty, tbl, subs, count, func, heapCount) = (ty, tbl, subs ++ [sub],count, func, heapCount)

typeTableInsert :: Type -> MyState -> MyState
typeTableInsert t (ty, tbl, subs,count,func, heapCount) = (ty, tbl ++ [t], subs,count,func, heapCount)

typeTableGet :: Token -> MyState -> Type
typeTableGet (Type _ name) (_, _, _,_,_,_)
    | name == "bool" = Type.Bool False
    | name == "int" = Type.Int 0
    | name == "real" = Type.Real 0.0
    | name == "char" = Type.Char ' '
    | name == "string" = Type.String ""
    | otherwise = error "Tipo primitivo não reconhecido"
typeTableGet (Id _ name) (_, tbl, _,_,_,_) = getUserDefinedType name tbl
typeTableGet t _ = error ("O token '"++show t++"' não representa um tipo definido pelo usuário")

getLogExprResult :: Type -> Bool
getLogExprResult (Type.Bool v) = v
getLogExprResult _ = error "O resultado da expressão não foi um valor booleano"


getUserDefinedType :: String -> [Type] -> Type
getUserDefinedType name [] = error ("O identificador '"++name++"' não representa um tipo")
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
getDefaultValue t = error ("O token '"++show t++"' não representa um tipo")

enterScope :: MyState -> MyState
enterScope (t, ty, programs,count,func, heapCount) = (t, ty, programs,count+1,func, heapCount)

exitScope :: MyState -> MyState
exitScope (t, ty, programs,count,func, heapCount) = (t, ty, programs,count-1,func, heapCount)

cleanVarsScope :: MyState -> MyState
cleanVarsScope (t, ty, programs,count,func, heapCount) = (removeVarsInScope (func++"."++show count) t, ty, programs, count, func, heapCount)

removeVarsInScope :: String -> [SymTable] -> [SymTable]
removeVarsInScope _ [] = []
removeVarsInScope prefix ((key, value, constFlag):t)
    | prefix `isPrefixOf` key = removeVarsInScope prefix t
    | otherwise = (key, value, constFlag):removeVarsInScope prefix t

callFunc :: String -> MyState -> MyState
callFunc name (t, ty, ref,count,_, heapCount) = (t, ty, ref,count,name, heapCount)

symtableGetValue :: String -> MyState -> SymTable
symtableGetValue name (t, _, _,count,func, _) = symtableGetInner (func,count,name) t t

symtableGetVar :: String -> MyState -> SymTable
symtableGetVar name (t, _, _,count,func,_) = symtableGetInner3 (func,count,name) t t

symtableGetInner :: (String,Int,String) -> [SymTable] -> [SymTable] -> SymTable
symtableGetInner (scope,count,name) ((key2, value, const1):t) backup
    | isRefType value = if key == key2 then symtableGetInner2 (getRefKey value) backup
                             else symtableGetInner (scope,count,name) t backup
    | otherwise = if key == key2 then (key2, value, const1)
                             else symtableGetInner (scope,count,name) t backup
    where key = scope++"."++show count++"."++name
symtableGetInner (scope,count,name) [] backup =
    if count > 0 then symtableGetInner (scope,count-1,name) backup backup
    else error ("Variável não declarada: '" ++ name++"'")

symtableGetInner2 :: String  -> [SymTable] -> SymTable
symtableGetInner2 key ((key2, value, const1):t) =
    if key == key2 then (key2, value, const1)
    else symtableGetInner2 key t
symtableGetInner2 key [] = error ("O endereço '"++key++" não é válido")

symtableGetInner3 :: (String,Int,String) -> [SymTable] -> [SymTable] -> SymTable
symtableGetInner3 (scope,count,name) ((key2, value, const1):t) backup
    = if key == key2 then (key2, value, const1)
                             else symtableGetInner3 (scope,count,name) t backup
    where key = scope++"."++show count++"."++name
symtableGetInner3 (scope,count,name) [] backup =
    if count > 0 then symtableGetInner3 (scope,count-1,name) backup backup
    else error ("Variável não declarada: '" ++ name++"'")

symtableInsert :: SymTable -> MyState -> MyState
symtableInsert (name, value, const1) (symtable, t, subs,count, func, heapCount)
    | varNameIsNew name func symtable  = symtableInsertInner (func++"."++show count++"."++name, value, const1) (symtable, t, subs,count, func, heapCount)
    | otherwise = error ("Não é possivel criar a variável '"++name++"' pois outra com o mesmo nome já existe.")

varNameIsNew :: String -> String -> [SymTable] -> Bool
varNameIsNew _ _ [] = True
varNameIsNew name1 func ((name2, _, _):vs)
    | ((func++".") `isPrefixOf` name2) && (("."++name1) `isSuffixOf` name2)= False
    | otherwise = varNameIsNew name1 func vs

symtableInsertInner :: SymTable -> MyState -> MyState
symtableInsertInner symbol ([], t, subs,count, func, heapCount) = ([symbol], t, subs,count, func, heapCount)
symtableInsertInner symbol (symtable, t, subs,count, func, heapCount) = (symbol:symtable, t, subs,count, func, heapCount)

symtableUpdate :: Bool -> Type -> SymTable -> MyState -> MyState
symtableUpdate x refVal tok (sym, ty, subs ,count, func, heapCount) = (symtableUpdateInner x refVal tok sym, ty, subs,count,func, heapCount)

symtableUpdateInner :: Bool -> Type -> SymTable -> [SymTable] -> [SymTable]
symtableUpdateInner _ _ (name, _, _) [] = error ("Variável não declarada: '" ++ name++"'")
symtableUpdateInner x refVal (name, value, const1) ((name2, value2, const2):t)
    | const1 = error ("Não se pode modificar uma constante: '"++name++"'")
    | x =   if getRefKey value == name2 then 
                if compatible refVal value2 then (name2, refVal, const2):t
                else error ("(ref on) Não é possivel guardar um " ++ show refVal ++" em um " ++ show value2)
            else (name2, value2, const2) : symtableUpdateInner x refVal (name, value, const1) t
    | otherwise =   if name == name2 then 
                        if compatible value value2 then (name, value, const1):t  
                        else error ("(ref off) Não é possivel guardar um " ++ show value ++" em um " ++ show value2)
                    else (name2, value2, const2) : symtableUpdateInner x refVal (name, value, const1) t

symtableRemove :: SymTable -> [SymTable] -> [SymTable]
symtableRemove (name, _, _) [] = error ("Variável não declarada: '" ++ name++"'")
symtableRemove (name, value, const1) ((name2, value2, const22):t) =
    if name == name2 then t
    else (name2, value2, const22) : symtableRemove (name, value, const1) t

symtableRemoveRef :: String -> MyState -> MyState
symtableRemoveRef key (symtable, t, subs,count, func, heapCount) =  (symtableRemoveRefInner key symtable, t, subs,count, func, heapCount)

symtableRemoveRefInner :: String -> [SymTable] -> [SymTable]
symtableRemoveRefInner key [] = error ("Variável não declarada: '" ++ key++"'")
symtableRemoveRefInner key ((name2, value2, const22):t) =
    if key == name2 then t
    else (name2, value2, const22) : symtableRemoveRefInner key t

getHeapId :: MyState -> Int 
getHeapId (_, _, _, _, _, heapCount) = heapCount

increaseHeapId :: MyState -> MyState 
increaseHeapId (tbl, typs, subsc, count, func, heapCount) = (tbl, typs, subsc, count, func, heapCount+1)

getProgramName :: MyState -> String 
getProgramName (_, _, _, _, func, _) = func

getCurrentScope :: MyState -> Int
getCurrentScope (_, _, _, scope, _, _) = scope

setCurrentScope :: Int -> MyState -> MyState
setCurrentScope count (tbl, typs, subsc, _, func, heapCount) = (tbl, typs, subsc, count, func, heapCount)