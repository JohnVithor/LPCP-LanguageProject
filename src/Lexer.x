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
  "create"                             { \p s -> Create p}
  "destroy"                            { \p s -> Destroy p}
  "ref"                                { \p s -> Ref p}
  "NULL"                               { \p s -> Null p}
  "begin"                              { \p s -> BeginScope p}
  "end"                                { \p s -> EndScope p}
  "constant"                           { \p s -> Constant p}
  "while"                              { \p s -> While p}
  "foreach"                            { \p s -> Foreach p}
  "repeat"                             { \p s -> Repeat p}
  "until"                              { \p s -> Until p}
  "break"                              { \p s -> Break p}
  "continue"                           { \p s -> Continue p}
  "return"                             { \p s -> Return p}
  "print"                              { \p s -> Print p}
  "newline"                            { \p s -> NewLine p}
  "read"                               { \p s -> Read p}
  "if"                                 { \p s -> If p}
  "else"                               { \p s -> Else p}
  "in"                                 { \p s -> In p}
  "is"                                 { \p s -> Is p}
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
  Create              AlexPosn	|
  Destroy             AlexPosn	|
  Ref                 AlexPosn	|
  Null                AlexPosn  |
  BeginScope          AlexPosn	|
  EndScope            AlexPosn	|
  Constant            AlexPosn	|
  Foreach             AlexPosn	|
  While               AlexPosn	|
  Repeat              AlexPosn	|
  Until               AlexPosn	|
  Break               AlexPosn	|
  Continue            AlexPosn	|
  Return              AlexPosn	|
  Print               AlexPosn	|
  NewLine             AlexPosn  |
  Read                AlexPosn	|
  If                  AlexPosn	|
  Else                AlexPosn	|
  In                  AlexPosn	|
  Is                  AlexPosn	|
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
token_posn (Struct           p) = p
token_posn (Create           p) = p
token_posn (Destroy          p) = p
token_posn (Ref              p) = p
token_posn (Null             p) = p
token_posn (BeginScope       p) = p
token_posn (EndScope         p) = p
token_posn (Constant         p) = p
token_posn (Foreach          p) = p
token_posn (While            p) = p
token_posn (Repeat           p) = p
token_posn (Until            p) = p
token_posn (Break            p) = p
token_posn (Continue         p) = p
token_posn (Return           p) = p
token_posn (Print            p) = p
token_posn (NewLine          p) = p
token_posn (Read             p) = p
token_posn (If               p) = p
token_posn (Else             p) = p
token_posn (In               p) = p
token_posn (Is               p) = p
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

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}
