module DataType where
import Lexer
import Type
import Data.Maybe
import Text.Parsec
import Control.Monad.IO.Class
import SymTable
import TokenParser
import Eval

dataType :: ParsecT [Token] MyState IO ([Token],Type)
dataType = do
        s <- getState
        t <- typeToken <|> idToken
        (r, v) <- listType (typeTableGet t s)
        return ([t], typeTableGet t s)

listType :: Type -> ParsecT [Token] MyState IO ([Token],Type)
listType t = try (do
        b <- beginListConstToken
        c <- endListConstToken
        (r, v) <- listType (Type.List t [])
        return (b:c:r, v)
        -- melhorar isso aqui depois
        ) <|> return ([], t)