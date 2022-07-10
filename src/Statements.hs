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

structCreation :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type, (Bool,String))
structCreation x = do
        a <- idToken
        s <- getState
        let ty = typeTableGet a s
        b <- beginExpressionToken
        (f,vs) <- args x
        d <- endExpressionToken
        if x then do
                return (a:b:f++[d], initStruct ty (extractTypes vs),(False,""))
        else return (a:b:f++ [d], Nothing,(False,"") )

extractTypes :: [Maybe Type] -> [Type]
extractTypes [] = []
extractTypes ((Just t):others) = t:extractTypes others
extractTypes (Nothing:_) = fail "Não foi possivel obter argumentos válidos"

initStruct :: Type -> [Type] -> Maybe Type
initStruct (Type.Struct name params) oargs = initStructInner (Type.Struct name params) params oargs
initStruct _ _ = fail "Não foi possível inicializar a struct"


initStructInner :: Type -> [(String,Type)] -> [Type] -> Maybe Type
initStructInner (Type.Struct name trueParams) [] [] = Just (Type.Struct name trueParams)
initStructInner (Type.Struct name trueParams) (param:params) (arg:oargs) = initStructInner (Type.Struct name (replaceArg trueParams param arg)) params oargs
initStructInner _ _ _ = fail "Não foi possível inicializar a struct com todos os parametors esperados"

replaceArg :: [(String,Type)] -> (String,Type) -> Type -> [(String,Type)]
replaceArg [] (name, _) _ = fail ("Argumento esperado não foi encontrado: " ++ name)
replaceArg ((expectedName,oldValue):trueArgs) (name,dValue) value = if expectedName == name then
                if compatible oldValue value then (name,value):trueArgs
                else error ("tipos incompatíveis na inicialização da struct: " ++ show oldValue ++ " " ++ show value)
        else (expectedName,oldValue):replaceArg trueArgs (name,dValue) value

args :: Bool -> ParsecT [Token] MyState IO ([Token], [Maybe Type])
args x = try (do
        (a, v,_) <- try(structCreation x) <|> expression x
        b <- commaToken
        (c, vs) <- args x
        return (a++b:c, v:vs))
        <|> do
        (a,v,_) <- try(structCreation x) <|> expression x
        return (a, [v])

initialization :: Bool -> Type -> ParsecT [Token] MyState IO ([Token], Maybe Type, (Bool,String))
 -- O par (Bool,String) indica se é ou não uma ref e qual a key da ref, no momento não temos ref
initialization x t = try (do
        c <- assignToken
        (d, r, (f,n)) <- try(structCreation x) <|> expression x <|> readStatement x <|> createInit x
        if x then do
                if compatible t (fromJust r) then return (c:d, r,(f,n))
                else fail ("Não são compatíveis: " ++ show t ++ " - " ++ show (fromJust r))
        else return (c:d, r,(False,"")))
        <|>
        (do
        return ([],Just t,(False,"")))

createInit :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type, (Bool,String))
createInit x = do
        a <- createToken 
        (b, r,_) <- try(structCreation x) <|> expression x
        if x then do
                s <- getState
                let name = "heap."++show (getHeapId s)
                updateState (symtableInsertInner (name, fromJust r, False, False, ""))
                return (a:b, r,(True,name))
        else return (a:b, r,(False,""))


varCreation :: Bool -> ParsecT [Token] MyState IO [Token]
varCreation x = do
            (a, t) <- dataType
            b <- idToken
            (c, v, (rf,rp)) <- try (initialization x t)
            if x then
                if not (compatible t (fromJust v)) then
                        fail ("Não são compatíveis: " ++ show t ++ " - " ++ show (fromJust v))
                else do
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
varAssignment x = do
        a <- idToken
        s <- getState
        if x then do
                let (key, oldValue, _, constFlag, _) = symtableGet (getIdData a) s
                (b, st, ns, ov) <- dotAccess x (Just oldValue)
                if constFlag then error "Não se pode alterar uma constante"
                else do
                        (c, v, (rf,rp)) <- initialization x (fromJust ov)
                        -- o que fazer com esses valores de ref e sua key? são permitidos?
                        if compatible (fromJust ov) (fromJust v) then
                                do
                                let nv = updateStructs (extractTypes st) ns (fromJust v)
                                let var = (key, nv, rf, False, rp)
                                updateState(symtableUpdate var)
                                return (a:b++c)
                        else fail ("Não são compatíveis: " ++ show oldValue ++ " - " ++ show (fromJust v))
        else do
                (b, _, _, _) <- dotAccess x Nothing
                (c, _, _) <- initialization x (Type.Bool False)
                return (a:b++c)

updateStructs :: [Type] -> [String] -> Type -> Type
updateStructs [] [] v = v
updateStructs (st:ss) (name:ts) v = fromJust (initStructInner st [(name,nv)] [nv])
        where nv = updateStructs ss ts v
updateStructs a b c = error ("debug: "++show a++" "++show b++" "++show c)



returnCall :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
returnCall x = do
        -- TODO: pensar em como voltar para o lugar que chamou a função e devolver esse valor
        a <- returnToken
        (tk, tp,_) <- expression x
        return (a:tk,tp)
        -- expression

destroyCall :: Bool -> ParsecT [Token] MyState IO [Token]
destroyCall _ = do
        a <- destroyToken
        b <- idToken
        -- TODO: remover id da heap
        return (a : [b])

statements :: Bool -> ParsecT [Token] MyState IO [Token]
statements x = (do
        c <- statement x
        d <- semiColonToken <|> anyToken 
        s <- getState
        liftIO (print (getSymbolTbl s))
        e <- statements x
        return (c++[d]++e)
        -- if isSemiColon d then do
        --         e <- statements x
        --         return (c++[d]++e)
        -- else do
        --         liftIO (print d)
        --         error "Não está faltando um ';' ?"
        ) <|> return []

isSemiColon :: Token -> Bool 
isSemiColon (SemiColon _) = True 
isSemiColon _ = False

statement :: Bool -> ParsecT [Token] MyState IO [Token]
statement x =
        printStatement x
        <|> try (varCreation x)
        <|> try (varAssignment x)
        <|> try (ifConditional x)
        <|> whileLoop x
        -- <|> try (procedureCall x)
        -- <|> try (returnCall x)
        -- <|> try (destroyCall x)
        -- <|> try (continueToken x)
        -- <|> breakToken x

printStatement :: Bool -> ParsecT [Token] MyState IO [Token]
printStatement x = do
        a <- printToken
        b <- beginExpressionToken
        (c, v,_) <- expression x
        d <- endExpressionToken
        if x then do
                liftIO (printVal (fromJust v))
                liftIO (putStrLn "")
                return (a:b:c++[d])
        else return (a:b:c++[d])

readStatement :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type,(Bool,String))
readStatement x = do
        a <- readToken
        if x then return ([a], Just (Type.String (unsafePerformIO getLine)),(False,""))
        else return ([a], Nothing,(False,""))


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
                    updateState cleanVarsScope
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
                if x then do
                        updateState enterScope
                        d <- statements x
                        updateState cleanVarsScope
                        updateState exitScope
                        e <- endScopeToken
                        f <- elseToken
                        return (a:b:c:d++e:[f])
                else do
                        d <- statements x
                        e <- endScopeToken
                        f <- elseToken
                        return (a:b:c:d++e:[f]))
                <|> do 
                        return []

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
