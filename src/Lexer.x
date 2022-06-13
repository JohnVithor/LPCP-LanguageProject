{
module Lexer where

import System.IO
import System.IO.Unsafe
}
%wrapper "posn"

$digit = [0-9]     -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-
  $white+                              ;
  "//".*                               ; -- comment
  "global"                             { \p s -> Global p}
  "struct"                             { \p s -> Struct p}
  "begin"                              { \p s -> BeginScope p}
  "end"                                { \p s -> EndScope p}
  "const"                              { \p s -> Const p}
  "switch"                             { \p s -> Switch p}
  "case"                               { \p s -> Case p}
  "if"                                 { \p s -> If p}
  "else"                               { \p s -> Else p}
  "ref"                                { \p s -> Ref p}
  "in"                                 { \p s -> In p}
  "and"                                { \p s -> And p}
  "or"                                 { \p s -> Or p}
  "not"                                { \p s -> Not p}
  "("                                  { \p s -> BeginExpression p}
  ")"                                  { \p s -> EndExpression p}
  "{"                                  { \p s -> BeginDeclaration p}
  "}"                                  { \p s -> EndDeclaration p}
  "["                                  { \p s -> BeginListConst p}
  "]"                                  { \p s -> EndListConst p}
  "."                                  { \p s -> Dot p}
  ","                                  { \p s -> Comma p}
  ":"                                  { \p s -> Colon p}
  ";"                                  { \p s -> SemiColon p}
  "="                                  { \p s -> Assign p}
  ">"                                  { \p s -> Greater p}
  "<"                                  { \p s -> Less p}
  "+"                                  { \p s -> Plus p}
  "-"                                  { \p s -> Minus p}
  "*"                                  { \p s -> Mult p}
  "/"                                  { \p s -> Div p}
  "%"                                  { \p s -> Mod p}
  "=="                                 { \p s -> Equal p}
  "<="                                 { \p s -> LessOrEqual p}
  ">="                                 { \p s -> GreaterOrEqual p}
  "!="                                 { \p s -> NotEqual p}
  "(real)"                             { \p s -> CastingReal p}
  "(char)"                             { \p s -> CastingChar p}
  "(bool)"                             { \p s -> CastingBool p}
  "(string)"                           { \p s -> CastingString p}
  "(int)"                              { \p s -> CastingInt p}
  "bool"                               { \p s -> Type p s}
  "int"                                { \p s -> Type p s}
  "real"                               { \p s -> Type p s}
  "char"                               { \p s -> Type p s}
  "string"                             { \p s -> Type p s}
  "false"                              { \p s -> Bool p False}
  "true"                               { \p s -> Bool p True}
  $digit+                              { \p s -> Int p (read s) }
  $digit+\.$digit+                     { \p s -> Real p (read s) }
  \'[^\']\'                            { \p s -> Char p (s !! 1) }
  \"[^\"]*\"                           { \p s -> String p (init (tail s))}
  $alpha[$alpha$digit\_]*              { \p s -> Id p s }


{
-- The token type:
data Token =
  CastingBool         AlexPosn  |
  CastingReal         AlexPosn  |
  CastingChar         AlexPosn  |
  CastingString       AlexPosn  |
  CastingInt          AlexPosn  |
  Global              AlexPosn	|
  Struct              AlexPosn	|
  BeginScope          AlexPosn	|
  EndScope            AlexPosn	|
  Const               AlexPosn	|
  Switch              AlexPosn	|
  Case                AlexPosn	|
  If                  AlexPosn	|
  Else                AlexPosn	|
  Ref                 AlexPosn	|
  In                  AlexPosn	|
  And                 AlexPosn	|
  Or                  AlexPosn	|
  Not                 AlexPosn	|
  BeginExpression     AlexPosn	|
  EndExpression       AlexPosn	|
  BeginDeclaration    AlexPosn	|
  EndDeclaration      AlexPosn	|
  BeginListConst      AlexPosn	|
  EndListConst        AlexPosn  |
  Dot                 AlexPosn	|
  Comma               AlexPosn	|
  Colon               AlexPosn	|
  SemiColon           AlexPosn	|
  Assign              AlexPosn	|
  Greater             AlexPosn	|
  Less                AlexPosn	|
  Plus                AlexPosn	|
  Minus               AlexPosn	|
  Mult                AlexPosn	|
  Div                 AlexPosn	|
  GreaterOrEqual      AlexPosn	|
  LessOrEqual         AlexPosn	|
  NotEqual            AlexPosn	|
  Equal               AlexPosn	|
  Mod                 AlexPosn	|
  Type                AlexPosn String  |
  Bool                AlexPosn Bool    |
  Int                 AlexPosn Int     |
  Real                AlexPosn Double  |
  Char                AlexPosn Char    |
  String              AlexPosn String  |
  Id                  AlexPosn String
  deriving (Eq,Show)

tokenPosn (CastingString    p) = p
tokenPosn (CastingChar      p) = p
tokenPosn (CastingBool      p) = p
tokenPosn (CastingReal      p) = p
tokenPosn (CastingInt       p) = p
tokenPosn (Global           p) = p
tokenPosn (Struct           p) = p
tokenPosn (BeginScope       p) = p
tokenPosn (EndScope         p) = p
tokenPosn (Const            p) = p
tokenPosn (Switch           p) = p
tokenPosn (Case             p) = p
tokenPosn (If               p) = p
tokenPosn (Else             p) = p
tokenPosn (Ref              p) = p
tokenPosn (In               p) = p
tokenPosn (And              p) = p
tokenPosn (Or               p) = p
tokenPosn (Not              p) = p
tokenPosn (BeginExpression  p) = p
tokenPosn (EndExpression    p) = p
tokenPosn (BeginDeclaration p) = p
tokenPosn (EndDeclaration   p) = p
tokenPosn (BeginListConst   p) = p
tokenPosn (Dot              p) = p
tokenPosn (Comma            p) = p
tokenPosn (Colon            p) = p
tokenPosn (SemiColon        p) = p
tokenPosn (Assign           p) = p
tokenPosn (Greater          p) = p
tokenPosn (Less             p) = p
tokenPosn (Plus             p) = p
tokenPosn (Minus            p) = p
tokenPosn (Mult             p) = p
tokenPosn (Div              p) = p
tokenPosn (Equal            p) = p
tokenPosn (NotEqual         p) = p
tokenPosn (GreaterOrEqual   p) = p
tokenPosn (LessOrEqual      p) = p
tokenPosn (Mod              p) = p
tokenPosn (Type           p _) = p
tokenPosn (Bool           p _) = p
tokenPosn (Int            p _) = p
tokenPosn (Real           p _) = p
tokenPosn (Char           p _) = p
tokenPosn (String         p _) = p
tokenPosn (Id             p _) = p

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
