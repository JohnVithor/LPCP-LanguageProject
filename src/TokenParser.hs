module TokenParser where

import Lexer
import Text.Parsec
import Type

updatePos :: p1 -> p2 -> [a] -> p1
updatePos pos _ _ = pos -- necessita melhoria

getIdData :: Token -> String
getIdData (Id _ x) = x
getIdData a = error ("Not an Id token "++show a)

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

beginExpressionToken :: ParsecT [Token] u IO Token
beginExpressionToken = tokenPrim show updatePos get_token where
  get_token (BeginExpression p) = Just (BeginExpression p)
  get_token _          = Nothing

endExpressionToken :: ParsecT [Token] u IO Token
endExpressionToken = tokenPrim show updatePos get_token where
  get_token (EndExpression p) = Just (EndExpression p)
  get_token _          = Nothing

colonToken :: ParsecT [Token] u IO Token
colonToken = tokenPrim show updatePos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _          = Nothing

commaToken :: ParsecT [Token] u IO Token
commaToken = tokenPrim show updatePos get_token where
  get_token (Comma p) = Just (Comma p)
  get_token _          = Nothing

printToken :: ParsecT [Token] u IO Token
printToken = tokenPrim show updatePos get_token where
  get_token (Print p) = Just (Print p)
  get_token _          = Nothing

readToken :: ParsecT [Token] u IO Token
readToken = tokenPrim show updatePos get_token where
  get_token (Read p) = Just (Read p)
  get_token _          = Nothing

beginListConstToken :: ParsecT [Token] u IO Token
beginListConstToken = tokenPrim show updatePos get_token where
  get_token (BeginListConst p) = Just (BeginListConst p)
  get_token _          = Nothing

endListConstToken :: ParsecT [Token] u IO Token
endListConstToken = tokenPrim show updatePos get_token where
  get_token (EndListConst p) = Just (EndListConst p)
  get_token _          = Nothing

intToken :: ParsecT [Token] u IO (Token, Type)
intToken = tokenPrim show updatePos get_token where
  get_token (Lexer.Int p x) = Just (Lexer.Int p x, Type.Int x)
  get_token _       = Nothing

stringToken :: ParsecT [Token] u IO (Token, Type)
stringToken = tokenPrim show updatePos get_token where
  get_token (Lexer.String p x) = Just (Lexer.String p x, Type.String x)
  get_token _       = Nothing

realToken :: ParsecT [Token] u IO (Token, Type)
realToken = tokenPrim show updatePos get_token where
  get_token (Lexer.Real p x) = Just (Lexer.Real p x,Type.Real x)
  get_token _       = Nothing

charToken :: ParsecT [Token] u IO (Token, Type)
charToken = tokenPrim show updatePos get_token where
  get_token (Lexer.Char p x) = Just (Lexer.Char p x, Type.Char x)
  get_token _       = Nothing

boolToken :: ParsecT [Token] u IO (Token, Type)
boolToken = tokenPrim show updatePos get_token where
  get_token (Lexer.Bool p x) = Just (Lexer.Bool p x, Type.Bool x)
  get_token _       = Nothing

constantToken :: ParsecT [Token] u IO Token
constantToken = tokenPrim show updatePos get_token where
  get_token (Constant p) = Just (Constant p)
  get_token _        = Nothing

semiColonToken :: ParsecT [Token] u IO Token
semiColonToken = tokenPrim show updatePos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _         = Nothing

dotToken :: ParsecT [Token] u IO Token
dotToken = tokenPrim show updatePos get_token where
  get_token (Dot p) = Just (Dot p)
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

minusToken :: ParsecT [Token] u IO Token
minusToken = tokenPrim show updatePos get_token where
  get_token (Minus p) = Just (Minus p)
  get_token _      = Nothing

multToken :: ParsecT [Token] u IO Token
multToken = tokenPrim show updatePos get_token where
  get_token (Mult p) = Just (Mult p)
  get_token _      = Nothing

divToken :: ParsecT [Token] u IO Token
divToken = tokenPrim show updatePos get_token where
  get_token (Div p) = Just (Div p)
  get_token _      = Nothing

modToken :: ParsecT [Token] u IO Token
modToken = tokenPrim show updatePos get_token where
  get_token (Mod p) = Just (Mod p)
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

foreachToken :: ParsecT [Token] u IO Token
foreachToken = tokenPrim show updatePos get_token where
  get_token (Foreach p) = Just (Foreach p)
  get_token _      = Nothing

whileToken :: ParsecT [Token] u IO Token
whileToken = tokenPrim show updatePos get_token where
  get_token (While p) = Just (While p)
  get_token _      = Nothing

repeatToken :: ParsecT [Token] u IO Token
repeatToken = tokenPrim show updatePos get_token where
  get_token (Repeat p) = Just (Repeat p)
  get_token _      = Nothing

untilToken :: ParsecT [Token] u IO Token
untilToken = tokenPrim show updatePos get_token where
  get_token (Until p) = Just (Until p)
  get_token _      = Nothing

openParenthesesToken :: ParsecT [Token] u IO Token
openParenthesesToken = tokenPrim show updatePos get_token where
  get_token (BeginExpression p) = Just (BeginExpression p)
  get_token _      = Nothing

closeParenthesesToken :: ParsecT [Token] u IO Token
closeParenthesesToken = tokenPrim show updatePos get_token where
  get_token (EndExpression p) = Just (EndExpression p)
  get_token _      = Nothing

ifToken :: ParsecT [Token] u IO Token
ifToken = tokenPrim show updatePos get_token where
  get_token (If p) = Just (If p)
  get_token _      = Nothing

elseToken :: ParsecT [Token] u IO Token
elseToken = tokenPrim show updatePos get_token where
  get_token (Else p) = Just (Else p)
  get_token _      = Nothing

refToken :: ParsecT [Token] u IO Token
refToken = tokenPrim show updatePos get_token where
  get_token (Lexer.Ref p) = Just (Lexer.Ref p)
  get_token _      = Nothing

returnToken :: ParsecT [Token] u IO Token
returnToken = tokenPrim show updatePos get_token where
  get_token (Return p) = Just (Return p)
  get_token _      = Nothing

createToken :: ParsecT [Token] u IO Token
createToken = tokenPrim show updatePos get_token where
  get_token (Create p) = Just (Create p)
  get_token _      = Nothing

destroyToken :: ParsecT [Token] u IO Token
destroyToken = tokenPrim show updatePos get_token where
  get_token (Destroy p) = Just (Destroy p)
  get_token _      = Nothing

continueToken :: ParsecT [Token] u IO Token
continueToken = tokenPrim show updatePos get_token where
  get_token (Continue p) = Just (Continue p)
  get_token _      = Nothing

breakToken :: ParsecT [Token] u IO Token
breakToken = tokenPrim show updatePos get_token where
  get_token (Break p) = Just (Break p)
  get_token _      = Nothing

castingBoolToken :: ParsecT [Token] u IO Token
castingBoolToken = tokenPrim show updatePos get_token where
  get_token (CastingBool p) = Just (CastingBool p)
  get_token _      = Nothing
  
castingIntToken :: ParsecT [Token] u IO Token
castingIntToken = tokenPrim show updatePos get_token where
  get_token (CastingInt p) = Just (CastingInt p)
  get_token _      = Nothing

castingRealToken :: ParsecT [Token] u IO Token
castingRealToken = tokenPrim show updatePos get_token where
  get_token (CastingReal p) = Just (CastingReal p)
  get_token _      = Nothing

castingCharToken :: ParsecT [Token] u IO Token
castingCharToken = tokenPrim show updatePos get_token where
  get_token (CastingChar p) = Just (CastingChar p)
  get_token _      = Nothing

castingStringToken :: ParsecT [Token] u IO Token
castingStringToken = tokenPrim show updatePos get_token where
  get_token (CastingString p) = Just (CastingString p)
  get_token _      = Nothing