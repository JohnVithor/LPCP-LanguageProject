module SymTable where

import Lexer

getDefaultValue :: Token -> Token
getDefaultValue (Type p "int") = Int p 0
getDefaultValue (Type p "real") = Real p 0.0
getDefaultValue (Type p "char") = Char p ' '
getDefaultValue (Type p "string") = String p ""
getDefaultValue (Type p "bool") = Bool p True
getDefaultValue _ = error "Is not a Type Token"

symtableInsert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtableInsert symbol []  = [symbol]
symtableInsert symbol symtable = symtable ++ [symbol]


symtableUpdate :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtableUpdate _ [] = fail "variable not found"
symtableUpdate (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then (id1, v1) : t
                               else (id2, v2) : symtableUpdate (id1, v1) t

symtableRemove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtableRemove _ [] = fail "variable not found"
symtableRemove (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else (id2, v2) : symtableRemove (id1, v1) t                               
