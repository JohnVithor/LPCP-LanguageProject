module Declarations where
import Lexer
import Type
import Text.Parsec
import SymTable
import TokenParser

-- listType :: Type -> ParsecT [Token] MyState IO ([Token],Type)
-- listType t = try (do
--         b <- beginListConstToken
--         c <- endListConstToken
--         (r, v) <- listType (Type.List t [])
--         return (b:c:r, v)
--         -- melhorar isso aqui depois
--         ) <|> return ([], t)