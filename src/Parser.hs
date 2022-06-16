module Parser where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

-- parsers para os terminais TODO:

globalToken = tokenPrim show updatePos get_token where
  get_token (Global p) = Just (Global p)
  get_token _          = Nothing

-- funcToken = tokenPrim show updatePos get_token where
--   get_token (Func p) = Just (Func p)
--   get_token _        = Nothing

-- ProcToken = tokenPrim show updatePos get_token where
--   get_token (Proc p) = Just (Proc p)
--   get_token _        = Nothing

-- StructToken = tokenPrim show updatePos get_token where
--   get_token (Struct p) = Just (Struct p)
--   get_token _          = Nothing

-- BeginScopeToken = tokenPrim show updatePos get_token where
--   get_token (BeginScope p) = Just (BeginScope p)
--   get_token _              = Nothing

-- EndScopeToken = tokenPrim show updatePos get_token where
--   get_token (EndScope p) = Just (EndScope p)
--   get_token _            = Nothing

-- ConstToken = tokenPrim show updatePos get_token where
--   get_token (Const p) = Just (Const p)
--   get_token _            = Nothing

-- SwitchToken = tokenPrim show updatePos get_token where
--   get_token (Switch p) = Just (Switch p)
--   get_token _            = Nothing

-- CaseToken = tokenPrim show updatePos get_token where
--   get_token (Case p) = Just (Case p)
--   get_token _            = Nothing

-- IfToken = tokenPrim show updatePos get_token where
--   get_token (If p) = Just (If p)
--   get_token _            = Nothing

-- ElseToken = tokenPrim show updatePos get_token where
--   get_token (Else p) = Just (Else p)
--   get_token _            = Nothing

-- RefToken = tokenPrim show updatePos get_token where
--   get_token (Ref p) = Just (Ref p)
--   get_token _            = Nothing

-- InToken = tokenPrim show updatePos get_token where
--   get_token (In p) = Just (In p)
--   get_token _            = Nothing

-- AndToken = tokenPrim show updatePos get_token where
--   get_token (And p) = Just (And p)
--   get_token _            = Nothing

-- OrToken = tokenPrim show updatePos get_token where
--   get_token (Or p) = Just (Or p)
--   get_token _            = Nothing

-- NotToken = tokenPrim show updatePos get_token where
--   get_token (Not p) = Just (Not p)
--   get_token _            = Nothing

-- BeginExpressionToken = tokenPrim show updatePos get_token where
--   get_token (BeginExpression p) = Just (BeginExpression p)
--   get_token _            = Nothing

-- EndExpressionToken = tokenPrim show updatePos get_token where
--   get_token (EndExpression p) = Just (EndExpression p)
--   get_token _            = Nothing

-- BeginDeclarationToken = tokenPrim show updatePos get_token where
--   get_token (BeginDeclaration p) = Just (BeginDeclaration p)
--   get_token _            = Nothing

-- EndDeclarationToken = tokenPrim show updatePos get_token where
--   get_token (EndDeclaration p) = Just (EndDeclaration p)
--   get_token _            = Nothing

-- BeginListConstToken = tokenPrim show updatePos get_token where
--   get_token (BeginListConst p) = Just (BeginListConst p)
--   get_token _            = Nothing

-- EndListConstToken = tokenPrim show updatePos get_token where
--   get_token (EndListConst p) = Just (EndListConst p)
--   get_token _            = Nothing

-- DotToken = tokenPrim show updatePos get_token where
--   get_token (Dot p) = Just (Dot p)
--   get_token _            = Nothing

-- CommaToken = tokenPrim show updatePos get_token where
--   get_token (Comma p) = Just (Comma p)
--   get_token _            = Nothing

-- ColonToken = tokenPrim show updatePos get_token where
--   get_token (Colon p) = Just (Colon p)
--   get_token _            = Nothing

-- SemiColonToken = tokenPrim show updatePos get_token where
--   get_token (SemiColon p) = Just (SemiColon p)
--   get_token _            = Nothing

-- AssignToken = tokenPrim show updatePos get_token where
--   get_token (Assign p) = Just (Assign p)
--   get_token _            = Nothing

-- GreaterToken = tokenPrim show updatePos get_token where
--   get_token (Greater p) = Just (Greater p)
--   get_token _            = Nothing

-- LessToken = tokenPrim show updatePos get_token where
--   get_token (Less p) = Just (Less p)
--   get_token _            = Nothing

-- PlusToken = tokenPrim show updatePos get_token where
--   get_token (Plus p) = Just (Plus p)
--   get_token _            = Nothing

-- MinusToken = tokenPrim show updatePos get_token where
--   get_token (Minus p) = Just (Minus p)
--   get_token _            = Nothing

-- MultToken = tokenPrim show updatePos get_token where
--   get_token (Mult p) = Just (Mult p)
--   get_token _            = Nothing

-- DivToken = tokenPrim show updatePos get_token where
--   get_token (Div p) = Just (Div p)
--   get_token _            = Nothing

-- ModToken = tokenPrim show updatePos get_token where
--   get_token (Mod p) = Just (Mod p)
--   get_token _            = Nothing

-- EqualToken = tokenPrim show updatePos get_token where
--   get_token (Equal p) = Just (Equal p)
--   get_token _            = Nothing

-- LessOrEqualToken = tokenPrim show updatePos get_token where
--   get_token (LessOrEqual p) = Just (LessOrEqual p)
--   get_token _            = Nothing

-- GreaterOrEqualToken = tokenPrim show updatePos get_token where
--   get_token (GreaterOrEqual p) = Just (GreaterOrEqual p)
--   get_token _            = Nothing

-- NotEqualToken = tokenPrim show updatePos get_token where
--   get_token (NotEqual p) = Just (NotEqual p)
--   get_token _            = Nothing

-- CastingRealToken = tokenPrim show updatePos get_token where
--   get_token (CastingReal p) = Just (CastingReal p)
--   get_token _            = Nothing

-- CastingCharToken = tokenPrim show updatePos get_token where
--   get_token (CastingChar p) = Just (CastingChar p)
--   get_token _            = Nothing

-- CastingBoolToken = tokenPrim show updatePos get_token where
--   get_token (CastingBool p) = Just (CastingBool p)
--   get_token _            = Nothing

-- CastingStringToken = tokenPrim show updatePos get_token where
--   get_token (CastingString p) = Just (CastingString p)
--   get_token _            = Nothing

-- CastingIntToken = tokenPrim show updatePos get_token where
--   get_token (CastingInt p) = Just (CastingInt p)
--   get_token _            = Nothing

-- CastingIntToken = tokenPrim show updatePos get_token where
--   get_token (CastingInt p) = Just (CastingInt p)
--   get_token _            = Nothing

-- -- 3 args

-- TypeToken = tokenPrim show updatePos get_token where
--   get_token (Type p v) = Just (Type p v)
--   get_token _            = Nothing

-- BoolToken = tokenPrim show updatePos get_token where
--   get_token (Bool p v) = Just (Bool p v)
--   get_token _            = Nothing

-- IntToken = tokenPrim show updatePos get_token where
--   get_token (Int p v) = Just (Int p v)
--   get_token _            = Nothing

-- RealToken = tokenPrim show updatePos get_token where
--   get_token (Real p v) = Just (Real p v)
--   get_token _            = Nothing

-- CharToken = tokenPrim show updatePos get_token where
--   get_token (Char p v) = Just (Char p v)
--   get_token _            = Nothing

-- StringToken = tokenPrim show updatePos get_token where
--   get_token (String p v) = Just (String p v)
--   get_token _            = Nothing

-- IdToken = tokenPrim show updatePos get_token where
--   get_token (Id p v) = Just (Id p v)
--   get_token _            = Nothing

-- auxiliar:

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos _ (tok:_) = pos -- necessita melhoria
updatePos pos _ []      = pos

-- parsers para os não-terminais TODO:

program :: ParsecT [Token] [(Token,Token)] IO [Token]
program = do
            a <- globalToken
            -- b <- funcToken
            -- b <- struc_declarations
            -- c <- subprograms
            eof
            return [a]

-- stmts :: Parsec [Token] st [Token]
-- stmts = do
--           first <- assign
--           next <- remainingStmts
--           return (first ++ next)

-- assign :: Parsec [Token] st [Token]
-- assign = do
--           a <- idToken
--           b <- assignToken
--           c <- intToken
--           return (a:b:[c])

-- remainingStmts :: Parsec [Token] st [Token]
-- remainingStmts = (do a <- semiColonToken
--                      b <- assign
--                      return (a:b)) <|> return []

-- invocação do parser para o símbolo de partida 


parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program [] "Error message"

