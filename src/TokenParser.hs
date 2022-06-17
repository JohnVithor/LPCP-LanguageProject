module TokenParser where

import Lexer
import Text.Parsec
import Type

updatePos :: p1 -> p2 -> [a] -> p1
updatePos pos _ (tok:_) = pos -- necessita melhoria
updatePos pos _ []      = pos

getIdData :: Token -> String
getIdData (Id _ x) = x
getIdData _ = error "Not an Id token"

globalToken :: ParsecT [Token] u IO Token
globalToken = tokenPrim show updatePos get_token where
  get_token (Global p) = Just (Global p)
  get_token _          = Nothing

structToken :: ParsecT [Token] u IO Token
structToken = tokenPrim show updatePos get_token where
  get_token (Lexer.Struct p) = Just (Lexer.Struct p)
  get_token _          = Nothing

beginScopeToken :: ParsecT [Token] u IO Token
beginScopeToken = tokenPrim show updatePos get_token where
  get_token (BeginScope p) = Just (BeginScope p)
  get_token _          = Nothing

endScopeToken :: ParsecT [Token] u IO Token
endScopeToken = tokenPrim show updatePos get_token where
  get_token (EndScope p) = Just (EndScope p)
  get_token _          = Nothing

colonToken :: ParsecT [Token] u IO Token
colonToken = tokenPrim show updatePos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _          = Nothing

beginListConstToken :: ParsecT [Token] u IO Token
beginListConstToken = tokenPrim show updatePos get_token where
  get_token (BeginListConst p) = Just (BeginListConst p)
  get_token _          = Nothing

endListConstToken :: ParsecT [Token] u IO Token
endListConstToken = tokenPrim show updatePos get_token where
  get_token (EndListConst p) = Just (EndListConst p)
  get_token _          = Nothing

intToken :: ParsecT [Token] u IO Type
intToken = tokenPrim show updatePos get_token where
  get_token (Lexer.Int p x) = Just (Type.Int x)
  get_token _       = Nothing

stringToken :: ParsecT [Token] u IO Type
stringToken = tokenPrim show updatePos get_token where
  get_token (Lexer.String p x) = Just (Type.String x)
  get_token _       = Nothing

realToken :: ParsecT [Token] u IO Type
realToken = tokenPrim show updatePos get_token where
  get_token (Lexer.Real p x) = Just (Type.Real x)
  get_token _       = Nothing

charToken :: ParsecT [Token] u IO Type
charToken = tokenPrim show updatePos get_token where
  get_token (Lexer.Char p x) = Just (Type.Char x)
  get_token _       = Nothing

boolToken :: ParsecT [Token] u IO Type
boolToken = tokenPrim show updatePos get_token where
  get_token (Lexer.Bool p x) = Just (Type.Bool x)
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