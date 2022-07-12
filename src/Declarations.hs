module Declarations where
import Lexer
import Type
import Text.Parsec
import SymTable
import TokenParser

structDeclaration :: ParsecT [Token] MyState IO [Token]
structDeclaration = do
            b <- structToken
            c <- idToken
            d <- colonToken
            (e, fields) <- fieldCreations
            f <- endScopeToken
            g <- idToken
            if getIdData c /= getIdData g then error "Nome da struct não é o mesmo"
            else
                do
                h <- semiColonToken
                updateState(typeTableInsert (Type.Struct (getIdData c) fields))
                return (b:c:d:e++[f,g,h])

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
            c <- semiColonToken 
            return (a++b:[c],(getIdData b, t))

dataType :: ParsecT [Token] MyState IO ([Token],Type)
dataType = refDataType <|> do
        s <- getState
        t <- typeToken <|> idToken
        -- (r, v) <- listType (typeTableGet t s)
        let v = typeTableGet t s
        return ([t], v)

refDataType :: ParsecT [Token] MyState IO ([Token],Type)
refDataType = do
        r <- refToken 
        t <- typeToken <|> idToken
        -- (ts, v) <- listType (typeTableGet t s)
        return (r:[t], Type.Ref (getNameOfThat t) "")

getNameOfThat :: Token -> String 
getNameOfThat (Type _ x) = x
getNameOfThat (Id _ x) = x
getNameOfThat _ = error "Não é válido"

-- listType :: Type -> ParsecT [Token] MyState IO ([Token],Type)
-- listType t = try (do
--         b <- beginListConstToken
--         c <- endListConstToken
--         (r, v) <- listType (Type.List t [])
--         return (b:c:r, v)
--         -- melhorar isso aqui depois
--         ) <|> return ([], t)