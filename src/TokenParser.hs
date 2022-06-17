module TokenParser where

import Lexer
import Text.Parsec

updatePos :: p1 -> p2 -> [a] -> p1
updatePos pos _ (tok:_) = pos -- necessita melhoria
updatePos pos _ []      = pos

globalToken :: ParsecT [Token] u IO Token
globalToken = tokenPrim show updatePos get_token where
  get_token (Global p) = Just (Global p)
  get_token _          = Nothing

intToken :: ParsecT [Token] u IO Token
intToken = tokenPrim show updatePos get_token where
  get_token (Int p x) = Just (Int p x)
  get_token _       = Nothing

stringToken :: ParsecT [Token] u IO Token
stringToken = tokenPrim show updatePos get_token where
  get_token (String p x) = Just (String p x)
  get_token _       = Nothing

realToken :: ParsecT [Token] u IO Token
realToken = tokenPrim show updatePos get_token where
  get_token (Real p x) = Just (Real p x)
  get_token _       = Nothing

charToken :: ParsecT [Token] u IO Token
charToken = tokenPrim show updatePos get_token where
  get_token (Char p x) = Just (Char p x)
  get_token _       = Nothing

boolToken :: ParsecT [Token] u IO Token
boolToken = tokenPrim show updatePos get_token where
  get_token (Bool p x) = Just (Bool p x)
  get_token _       = Nothing

constantToken :: ParsecT [Token] u IO Token
constantToken = tokenPrim show updatePos get_token where
  get_token (Constant p) = Just (Constant p)
  get_token _        = Nothing

semiColonToken :: ParsecT [Token] u IO Token
semiColonToken = tokenPrim show updatePos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _         = Nothing

assignToken :: ParsecT [Token] u IO Token
assignToken = tokenPrim show updatePos get_token where
  get_token (Assign p) = Just (Assign p)
  get_token _      = Nothing

idToken :: ParsecT [Token] u IO Token
idToken = tokenPrim show updatePos get_token where
  get_token (Id p x) = Just (Id p x)
  get_token _      = Nothing

typeToken :: ParsecT [Token] u IO Token
typeToken = tokenPrim show updatePos get_token where
  get_token (Type p x) = Just (Type p x)
  get_token _        = Nothing

plusToken :: ParsecT [Token] u IO Token
plusToken = tokenPrim show updatePos get_token where
  get_token (Plus p) = Just (Plus p)
  get_token _      = Nothing

orToken :: ParsecT [Token] u IO Token
orToken = tokenPrim show updatePos get_token where
  get_token (Or p) = Just (Or p)
  get_token _      = Nothing

andToken :: ParsecT [Token] u IO Token
andToken = tokenPrim show updatePos get_token where
  get_token (And p) = Just (And p)
  get_token _      = Nothing

notToken :: ParsecT [Token] u IO Token
notToken = tokenPrim show updatePos get_token where
  get_token (Not p) = Just (Not p)
  get_token _      = Nothing

equalToken :: ParsecT [Token] u IO Token
equalToken = tokenPrim show updatePos get_token where
  get_token (Equal p) = Just (Equal p)
  get_token _      = Nothing

lessOrEqualToken :: ParsecT [Token] u IO Token
lessOrEqualToken = tokenPrim show updatePos get_token where
  get_token (LessOrEqual p) = Just (LessOrEqual p)
  get_token _      = Nothing

greaterOrEqualToken :: ParsecT [Token] u IO Token
greaterOrEqualToken = tokenPrim show updatePos get_token where
  get_token (GreaterOrEqual p) = Just (GreaterOrEqual p)
  get_token _      = Nothing

notEqualToken :: ParsecT [Token] u IO Token
notEqualToken = tokenPrim show updatePos get_token where
  get_token (NotEqual p) = Just (NotEqual p)
  get_token _      = Nothing

lessToken :: ParsecT [Token] u IO Token
lessToken = tokenPrim show updatePos get_token where
  get_token (Less p) = Just (Less p)
  get_token _      = Nothing

greaterToken :: ParsecT [Token] u IO Token
greaterToken = tokenPrim show updatePos get_token where
  get_token (Greater p) = Just (Greater p)
  get_token _      = Nothing

inToken :: ParsecT [Token] u IO Token
inToken = tokenPrim show updatePos get_token where
  get_token (In p) = Just (In p)
  get_token _      = Nothing

isToken :: ParsecT [Token] u IO Token
isToken = tokenPrim show updatePos get_token where
  get_token (Is p) = Just (Is p)
  get_token _      = Nothing