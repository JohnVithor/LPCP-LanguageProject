module Parser where

import Lexer
import Text.Parsec
import TokenParser
import SymTable
import Type
import Statements
import Subprograms
import Eval

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
                (a,_) <- varCreation True -- <|> constantDecl
                b <- semiColonToken 
                c <- globalVarCreations
                return (a++b:c))
                <|> return []

declaration :: ParsecT [Token] MyState IO [Token]
declaration = do
        a <- beginScopeToken
        b <- structDeclaration <|> try (subprogramCreation Nothing) <|> functionCreation
        return (a:b)

declarations :: ParsecT [Token] MyState IO [Token]
declarations = (do
                c <- declaration
                e <- declarations
                return (c++e))
                <|> return []

literal :: ParsecT [Token] MyState IO (Token,Type)
literal = intToken <|> realToken <|> charToken <|> stringToken <|> boolToken

program :: ParsecT [Token] MyState IO [Token]
program = do
        _ <- globalVars
        _ <- declarations
        eof       
        s <- getState
        setInput (getStmts (getMainFunc s))
        updateState (callFunc "main")
        _ <- statements True
        updateState cleanVarsScope
        -- s <- getState
        -- liftIO(print s)
        return []


parser :: [Token] -> IO (Either ParseError [Token])
parser = runParserT program ([], [], [],0,"global",0) "Error message"
