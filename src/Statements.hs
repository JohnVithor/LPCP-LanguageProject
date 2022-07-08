module Statements where
import Lexer
import Type
import Data.Maybe
import Text.Parsec
import Control.Monad.IO.Class
import SymTable
import TokenParser
import Declarations
import Expressions
import GHC.IO.Unsafe (unsafePerformIO)

structCreation :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
structCreation x = do
        a <- idToken
        b <- beginExpressionToken
        (f,vs) <- args x
        d <- endExpressionToken
        s <- getState
        if x then do
                return (a:b:f++[d], initStruct (typeTableGet a s) (extractArgs vs))
        else return (a:b:f++ [d], Nothing )

extractArgs :: [Maybe Type] -> [Type]
extractArgs [] = []
extractArgs ((Just t):others) = t:extractArgs others
extractArgs (Nothing:_) = fail "deu ruim"

initStruct :: Type -> [Type] -> Maybe Type
initStruct (Type.Struct name params) oargs = initStructInner (Type.Struct name params) params oargs
initStruct _ _ = fail "deu ruim outro caso de struct"


initStructInner :: Type -> [(String,Type)] -> [Type] -> Maybe Type
initStructInner (Type.Struct name trueParams) [] [] = Just (Type.Struct name trueParams)
initStructInner (Type.Struct name trueParams) (param:params) (arg:oargs) = initStructInner (Type.Struct name (replaceArg trueParams param arg)) params oargs
initStructInner _ _ _ = fail "deu ruim caso de struct"

replaceArg :: [(String,Type)] -> (String,Type) -> Type -> [(String,Type)]
replaceArg [] _ _ = fail "deu ruim: argumento não encontrado"
replaceArg ((expectedName,oldValue):trueArgs) (name,dValue) value = if expectedName == name then
                if compatible oldValue value then (name,value):trueArgs
                else error ("tipos incompatíveis na inicialização da struct: " ++ show oldValue ++ " " ++ show value)
        else (expectedName,oldValue):replaceArg trueArgs (name,dValue) value

args :: Bool -> ParsecT [Token] MyState IO ([Token], [Maybe Type])
args x = try (do
        (a, v) <- expression x
        b <- commaToken
        (c, vs) <- args x
        return (a++b:c, v:vs))
        <|> do
        (a,v) <- expression x
        return (a, [v])

initialization :: Bool -> Type -> ParsecT [Token] MyState IO ([Token], Maybe Type, (Bool,String))
 -- O par (Bool,String) indica se é ou não uma ref e qual a key da ref, no momento não temos ref
initialization x t = try (do
        c <- assignToken
        (d, r) <- try (readStatement x) <|> try (structCreation x) <|> expression x
        if x then do
                if compatible t (fromJust r) then return (c:d, r,(False,""))
                else fail "tipos diferentes"
        else return (c:d, r,(False,"")))
        <|>
        (do
        return ([],Just t,(False,"")))

varCreation :: Bool -> ParsecT [Token] MyState IO [Token]
varCreation x = do
            (a, t) <- dataType
            b <- idToken
            (c, v, (rf,rp)) <- try (initialization x t)
            if x then
                if not (compatible t (fromJust v)) then fail "tipos diferentes" else
                        do
                        -- o False abaixo é para constantes, no momento sem constantes
                        let var = (getIdData b, fromJust v, rf, False, rp)
                        updateState(symtableInsert var)
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
varAssignment x = try (do
        a <- idToken
        b <- dotToken
        c <- idToken
        s <- getState
        if x then do
                let (key, oldValue, refFlag, constFlag, refName) = symtableGet (getIdData a) s
                let fieldValue = getStructField oldValue (getIdData c)
                (e, v, (rf,rp)) <- initialization x fieldValue
                -- o que fazer com esses valores de ref e sua key? são permitidos?
                if compatible fieldValue (fromJust v) then
                        do
                        let newValue = fromJust (initStructInner oldValue [(getIdData c,fromJust v)] [fromJust v])
                        updateState(symtableUpdate (key, newValue, refFlag, constFlag, refName))
                        return (a:b:c:e)
                else fail "tipos diferentes"
        else do
                (e, _, _) <- initialization x (Type.Bool False)
                return (a:b:c:e)
        ) <|> try (do
        a <- idToken
        s <- getState
        if x then do
                let (key, oldValue, refFlag, constFlag, refName) = symtableGet (getIdData a) s
                (c, v, (rf,rp)) <- initialization x oldValue
                -- o que fazer com esses valores de ref e sua key? são permitidos?
                if compatible oldValue (fromJust v) then
                        do
                        updateState(symtableUpdate (key, fromJust v, refFlag, constFlag, refName))
                        return (a:c)
                else fail "tipos diferentes"
        else do
                (c, _, _) <- initialization x (Type.Bool False)
                return (a:c)
        )

returnCall :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
returnCall x = do
        -- TODO: pensar em como voltar para o lugar que chamou a função e devolver esse valor
        a <- returnToken
        (tk, tp) <- expression x
        return (a:tk,tp)
        -- expression

destroyCall :: Bool -> ParsecT [Token] MyState IO [Token]
destroyCall _ = do
        a <- destroyToken
        b <- idToken
        -- TODO: remover id da heap
        return (a : [b])

statements :: Bool -> ParsecT [Token] MyState IO [Token]
statements x = try (do
        c <- statement x
        d <- semiColonToken
        e <- statements x
        return (c++[d]++e))
        <|> return []

statement :: Bool -> ParsecT [Token] MyState IO [Token]
statement x =
        try (ifConditional x)
        <|> try (printStatement x)
        <|> try (whileLoop x)
        <|> try (varCreations x)
        <|> try (varAssignment x)
        -- <|> try (readStatement x)
        -- <|> try (loop x)
        -- <|> try (procedureCall x)
        -- <|> try (returnCall x)
        -- <|> try (destroyCall x)
        -- <|> try (continueToken x)
        -- <|> breakToken x

printStatement :: Bool -> ParsecT [Token] MyState IO [Token]
printStatement x = do
        a <- printToken
        b <- beginExpressionToken
        (c, v) <- expression x
        d <- endExpressionToken
        if x then do
                liftIO (printVal (fromJust v))
                liftIO (putStrLn "")
                return (a:b:c++[d])
        else return (a:b:c++[d])

{-# NOINLINE readStatement #-}
readStatement :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
readStatement x = do
        a <- readToken
        if x then return ([a], Just (Type.String (unsafePerformIO getLine)))
        else return ([a],Nothing )


-- <conditional> := begin if ( <logic_expression> ): <statements> end if
ifConditional :: Bool -> ParsecT [Token] MyState IO [Token]
ifConditional x = do
                a <- beginScopeToken
                b <- ifToken
                c <- openParenthesesToken
                (d,v) <- logExpr x
                e <- closeParenthesesToken
                f <- colonToken
                if x then do
                    let r = getLogExprResult (fromJust v)
                    updateState enterScope
                    g <- statements r
                    updateState exitScope
                    h <- endScopeToken
                    i <- ifToken
                    j <- elseConditional (not r)
                    return (a:b:c:d++e:f:g++h:i:j)
                else do
                    g <- statements x
                    h <- endScopeToken
                    i <- ifToken
                    j <- elseConditional x
                    return (a:b:c:d++e:f:g++h:i:j)

elseConditional :: Bool -> ParsecT [Token] MyState IO [Token]
elseConditional x = try (do
                a <- beginScopeToken
                b <- elseToken
                c <- colonToken
                updateState enterScope
                d <- statements x
                updateState exitScope
                e <- endScopeToken
                f <- elseToken
                return (a:b:c:d++e:[f]))
                <|> return []

-- begin while(logicExpression): stmts end while
whileLoop :: Bool -> ParsecT [Token] MyState IO [Token]
whileLoop x = do
                a <- beginScopeToken
                b <- whileToken
                c <- openParenthesesToken
                if x then do
                        (d,v) <- logExpr x
                        e <- closeParenthesesToken
                        f <- colonToken
                        let r = getLogExprResult (fromJust v)
                        updateState enterScope
                        g <- statements r
                        updateState exitScope
                        h <- endScopeToken
                        i <- whileToken
                        if r then do
                                inp <- getInput
                                setInput (a:b:c:d++e:f:g++h:[i])
                                fr <- whileLoop x
                                setInput inp
                                return fr
                        else do
                                return (a:b:c:d++e:f:g++h:[i])
                else do
                        (d,_) <- logExpr x
                        e <- closeParenthesesToken
                        f <- colonToken
                        g <- statements x
                        h <- endScopeToken
                        i <- whileToken
                        return (a:b:c:d++e:f:g++h:[i])

