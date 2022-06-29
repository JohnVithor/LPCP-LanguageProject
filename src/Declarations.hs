module Declarations where
import Lexer
import Type
import Data.Maybe
import Text.Parsec
import Control.Monad.IO.Class
import SymTable
import TokenParser
import Eval

declaration :: ParsecT [Token] MyState IO [Type]
declaration = do
        a <- beginScopeToken
        try structDeclaration
        --  <|> mainFunction
        --  <|> try functionCreation <|> subprogramCreation Nothing

declarations :: ParsecT [Token] MyState IO [Type]
declarations = (do
                c <- declaration
                e <- declarations
                return (c++e))
                <|> return []

structDeclaration :: ParsecT [Token] MyState IO [Type]
structDeclaration = do
            b <- structToken
            c <- idToken
            d <- colonToken
            (e, fields) <- fieldCreations
            f <- endScopeToken
            g <- idToken
            s <- getState
            if getIdData c /= getIdData g then fail "Nome da struct não é o mesmo"
            else
                do
                h <- semiColonToken
                updateState(typeTableInsert (Type.Struct (getIdData c) fields))
                return [Type.Struct (getIdData c) fields]

fieldCreations :: ParsecT [Token] MyState IO ([Token],[(String, Type)])
fieldCreations = (do
                    (c, f) <- fieldCreation
                    (cs, fs) <- fieldCreations
                    return (c++cs, f:fs))
                    <|> return ([], [])

fieldCreation :: ParsecT [Token] MyState IO ([Token],(String, Type))
fieldCreation = do
            (a, t) <- dataType
            b <- idToken
        --     (c, v) <- initialization a
            return (a++[b],(getIdData b, t))

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