module Statements where
import Lexer
import Type
import Data.Maybe
import Text.Parsec
import Control.Monad.IO.Class
import SymTable
import TokenParser
import Eval
import Declarations
import Expressions
import Data.Data

structCreation :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
structCreation x = do
        a <- idToken
        b <- beginExpressionToken
        f <- args x
        d <- endExpressionToken
        s <- getState
        if x then do
                return (a:b:[d], initStruct (typeTableGet a s) (extractArgs f))
        else return (a:b:[d], Nothing )

extractArgs :: [([Token], Maybe Type)] -> [Type]
extractArgs [] = []
extractArgs ((toks, Just t):others) = t:extractArgs others
extractArgs ((toks, Nothing):others) = fail "deu ruim"

initStruct :: Type -> [Type] -> Maybe Type
initStruct (Type.Struct name params) args = initStructInner (Type.Struct name params) params args
initStruct _ _ = fail "deu ruim outro caso de struct"


initStructInner :: Type -> [(String,Type)] -> [Type] -> Maybe Type
initStructInner (Type.Struct name trueParams) [] [] = Just (Type.Struct name trueParams)
initStructInner (Type.Struct name trueParams) (param:params) (arg:args) = initStructInner (Type.Struct name (replaceArg trueParams param arg)) params args
initStructInner _ _ _ = fail "deu ruim caso de struct"

replaceArg :: [(String,Type)] -> (String,Type) -> Type -> [(String,Type)]
replaceArg [] _ _ = fail "deu ruim: argumento não encontrado"
replaceArg ((expectedName,oldValue):trueArgs) (name,dValue) value = if expectedName == name then
                if compatible oldValue value then (name,value):trueArgs
                else error ("tipos incompatíveis na inicialização da struct: " ++ show oldValue ++ " " ++ show value)
        else (expectedName,oldValue):replaceArg trueArgs (name,dValue) value

args :: Bool -> ParsecT [Token] MyState IO [([Token], Maybe Type)]
args x = try (do
        a <- expression x
        b <- commaToken
        c <- args x
        return (a:c))
        <|> do
        a <- expression x
        return [a]

initialization :: Bool -> Type -> ParsecT [Token] MyState IO ([Token], Maybe Type)
initialization x t = try (do
        c <- assignToken
        (d, r) <- try (structCreation x) <|> expression x
        if x then do
                if compatible t (fromJust r) then return (c:d, r)
                else fail "tipos diferentes"
        else return (c:d, r))
        <|>
        (do
        return ([],Just t))

varCreation :: Bool -> ParsecT [Token] MyState IO [Token]
varCreation x = do
            (a, t) <- dataType
            b <- idToken
            (c, r) <- initialization x t
            if x then
                if not (compatible t (fromJust r)) then fail "tipos diferentes" else
                        do
                        s <- getState
                        updateState(symtableInsert (getIdData b, fromJust r))
                        return (a++[b]++c)
                else
                        return (a++[b]++c)

varCreations :: Bool -> ParsecT [Token] MyState IO [Token]
varCreations x = (do
                    c <- varCreation x -- <|> constantDecl
                    e <- varCreations x
                    return (c++e))
                    <|> return []

varAssignment :: Bool -> ParsecT [Token] MyState IO [Token]
varAssignment x = do
                a <- idToken
                b <- assignToken
                (c, v) <- expression x
                s <- getState
                d <- semiColonToken
                if x then
                        if compatible (fromJust (symtableGet (getIdData a) s)) (fromJust v) then
                                do
                                updateState(symtableUpdate (getIdData a, fromJust v))
                                return ([a]++[b]++c++[d])
                        else fail "tipos diferentes"
                else return ([a]++[b]++c++[d])

returnCall :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
returnCall x = do
        -- TODO: pensar em como voltar para o lugar que chamou a função e devolver esse valor
        a <- returnToken
        (tk, tp) <- expression x
        return (a:tk,tp)
        -- expression

destroyCall :: Bool -> ParsecT [Token] MyState IO [Token]
destroyCall x = do
        a <- destroyToken
        b <- idToken
        -- TODO: remover id da heap
        return (a : [b])

statements :: Bool -> ParsecT [Token] MyState IO [Token]
statements x = (do
        c <- statement x
        d <- semiColonToken
        e <- statements x
        -- s <- getState
        -- liftIO (print s)
        return (c++[d]++e))
        <|> return []

statement :: Bool -> ParsecT [Token] MyState IO [Token]
statement x = try (varCreations x)
        <|> try (varAssignment x)
        -- <|> try (conditional x)
        -- <|> try (loop x)
        -- <|> try (procedureCall x)
        -- <|> try (returnCall x)
        -- <|> try (destroyCall x)
        -- <|> try (continueToken x)
        -- <|> breakToken x
