{
  module Main (main, Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = [0-9]     -- digits
$alpha = [a-zA-Z]  -- alphabetic characters

tokens :-
  $white+                              ;
  "//".*                               ; -- comment
  "global"                             { \p s -> Global p}
  "func"                               { \p s -> Func p}
  "proc"                               { \p s -> Proc p}
  "struct"                             { \p s -> Struct p}
  "create"                             { \p s -> Create p}
  "destroy"                            { \p s -> Destroy p}
  "begin"                              { \p s -> BeginScope p}
  "end"                                { \p s -> EndScope p}
  "const"                              { \p s -> Const p}
  "switch"                             { \p s -> Switch p}
  "case"                               { \p s -> Case p}
  "if"                                 { \p s -> If p}
  "else"                               { \p s -> Else p}
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
  Func                AlexPosn	|
  Proc                AlexPosn	|
  Struct              AlexPosn	|
  Create              AlexPosn	|
  Destroy             AlexPosn	|
  BeginScope          AlexPosn	|
  EndScope            AlexPosn	|
  Const               AlexPosn	|
  Switch              AlexPosn	|
  Case                AlexPosn	|
  If                  AlexPosn	|
  Else                AlexPosn	|
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

token_posn (CastingString    p) = p
token_posn (CastingChar      p) = p
token_posn (CastingBool      p) = p
token_posn (CastingReal      p) = p
token_posn (CastingInt       p) = p
token_posn (Global           p) = p
token_posn (Func             p) = p
token_posn (Proc             p) = p
token_posn (Struct           p) = p
token_posn (Create           p) = p
token_posn (Destroy          p) = p
token_posn (BeginScope       p) = p
token_posn (EndScope         p) = p
token_posn (Const            p) = p
token_posn (Switch           p) = p
token_posn (Case             p) = p
token_posn (If               p) = p
token_posn (Else             p) = p
token_posn (In               p) = p
token_posn (And              p) = p
token_posn (Or               p) = p
token_posn (Not              p) = p
token_posn (BeginExpression  p) = p
token_posn (EndExpression    p) = p
token_posn (BeginDeclaration p) = p
token_posn (EndDeclaration   p) = p
token_posn (BeginListConst   p) = p
token_posn (Dot              p) = p
token_posn (Comma            p) = p
token_posn (Colon            p) = p
token_posn (SemiColon        p) = p
token_posn (Assign           p) = p
token_posn (Greater          p) = p
token_posn (Less             p) = p
token_posn (Plus             p) = p
token_posn (Minus            p) = p
token_posn (Mult             p) = p
token_posn (Div              p) = p
token_posn (Equal            p) = p
token_posn (NotEqual         p) = p
token_posn (GreaterOrEqual   p) = p
token_posn (LessOrEqual      p) = p
token_posn (Mod              p) = p
token_posn (Type           p _) = p
token_posn (Bool           p _) = p
token_posn (Int            p _) = p
token_posn (Real           p _) = p
token_posn (Char           p _) = p
token_posn (String         p _) = p
token_posn (Id             p _) = p

main = do
 s <- getContents
 print (alexScanTokens s)
}
