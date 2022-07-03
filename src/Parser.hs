module Parser where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import TokenParser
import SymTable
import Type
import Statements
import Declarations
import Subprograms

globalVars :: ParsecT [Token] MyState IO [Token]
globalVars = try (do
        a <- beginScopeToken
        b <- globalToken
        c <- colonToken
        d <- globalVarCreations
        e <- endScopeToken
        f <- globalToken
        g <- semiColonToken
        return (a:b:c:e:d ++ f:[g])) <|> return []

globalVarCreations :: ParsecT [Token] MyState IO [Token]
globalVarCreations = try (do
                a <- varCreation True -- <|> constantDecl
                b <- semiColonToken 
                c <- globalVarCreations
                return (a++b:c))
                <|> return []

declaration :: ParsecT [Token] MyState IO [Token]
declaration = do
        a <- beginScopeToken
        b <- try structDeclaration <|> mainFunction
        return (a:b)
        --  <|> try functionCreation <|> subprogramCreation Nothing

declarations :: ParsecT [Token] MyState IO [Token]
declarations = (do
                c <- declaration
                e <- declarations
                return (c++e))
                <|> return []

--parser para declaração de constante int
--constant int a = 10;
-- constantDecl :: ParsecT [Token] MyState IO Type
-- constantDecl = do
--             const <- constantToken
--             -- TODO: usar essa informação sobre ser constante
--             a <- typeToken
--             b <- idToken
--             c <- assignToken
--             d <- intToken <|> stringToken <|> charToken <|> realToken <|> boolToken
--             e <- semiColonToken
--             s <- getState
--             if not (compatible (typeTableGet a s) d) then fail "tipos diferentes" else
--                 do
--                 updateState(symtableInsert (getIdData b, d))
--                 return (Type.Bool False) -- O ideal seria não retornar nada.

literal :: ParsecT [Token] MyState IO (Token,Type)
literal = intToken<|> realToken <|> charToken <|> stringToken <|> boolToken

mainFunction :: ParsecT [Token] MyState IO [Token]
mainFunction = do
        b <- typeToken
        c <- idToken
        d <- beginExpressionToken
        (e, ps) <- params
        f <- endExpressionToken
        g <- colonToken
        h <- statements True
        i <- endScopeToken
        j <- idToken
        l <- semiColonToken
        if getIdData c /= getIdData j then fail "Nome do subprograma não é o mesmo"
        else
                if getIdData c /= "main" then fail "função main não encontrada" else
                do
                -- TODO: atualizar a lista de comandos !!!
                updateState(subsprogramTableInsert (getIdData c, Just (Type.Int 0), ps, [c]))
                return (b:c:d:e++f:g:h++[i,j,l])

program :: ParsecT [Token] MyState IO [Token]
program = do
        a <- globalVars
        b <- declarations
        eof
        s <- getState
        liftIO (print s)
        return (a++b)
        -- return (runFunc (getMainFunc s) [])

parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program ([], [], []) "Error message"
