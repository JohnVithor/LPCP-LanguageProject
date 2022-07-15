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
import Eval

structCreation :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
structCreation x = do
        a <- idToken
        s <- getState
        let ty = typeTableGet a s
        b <- beginExpressionToken
        (f,vs) <- args x
        d <- endExpressionToken
        if x then do
                return (a:b:f++[d], initStruct ty (extractTypes vs))
        else return (a:b:f++ [d], Nothing )

extractTypes :: [Maybe Type] -> [Type]
extractTypes [] = []
extractTypes ((Just t):others) = t:extractTypes others
extractTypes (Nothing:_) = error "Não foi possivel obter argumentos válidos"

initStruct :: Type -> [Type] -> Maybe Type
initStruct (Type.Struct name params) oargs = initStructInner (Type.Struct name params) params oargs
initStruct _ _ = error "Não foi possível inicializar a struct"


initStructInner :: Type -> [(String,Type)] -> [Type] -> Maybe Type
initStructInner (Type.Struct name trueParams) [] [] = Just (Type.Struct name trueParams)
initStructInner (Type.Struct name trueParams) (param:params) (arg:oargs) = initStructInner (Type.Struct name (replaceArg trueParams param arg)) params oargs
initStructInner a b c = error ("Não foi possível inicializar a struct com todos os parametors esperados: \n" ++ show a ++ " " ++ show b ++ " " ++ show c)

replaceArg :: [(String,Type)] -> (String,Type) -> Type -> [(String,Type)]
replaceArg [] (name, _) _ = error ("Argumento esperado não foi encontrado: " ++ name)
replaceArg ((expectedName,oldValue):trueArgs) (name,dValue) value = if expectedName == name then
                if compatible oldValue value then (name,value):trueArgs
                else error ("tipos incompatíveis na inicialização da struct: " ++ show oldValue ++ " " ++ show value)
        else (expectedName,oldValue):replaceArg trueArgs (name,dValue) value

args :: Bool -> ParsecT [Token] MyState IO ([Token], [Maybe Type])
args x = try (do
        (a, v) <- try(structCreation x) <|> expression x <|> refInitialization x
        b <- commaToken
        (c, vs) <- args x
        return (a++b:c, v:vs))
        <|> do
        (a,v) <- try(structCreation x) <|> expression x <|> refInitialization x
        return (a, [v])

initialization :: Bool -> Type -> ParsecT [Token] MyState IO ([Token], Maybe Type)
initialization x t = (do
        c <- assignToken
        (d, r) <- refInitialization x <|> try (funcCall x) <|> try(structCreation x) <|> expression x <|> readStatement x <|> createInit x
        return (c:d, r))
        <|>
        (do
        return ([],Just t))

refInitialization :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
refInitialization x = do
        a <- refToken
        (d, r) <- getVar x True
        if x then do
                let v = fromJust r
                if isRefType v then return (a:d, Just v)
                else error ("Não é do Tipo Ref!: " ++ show v)
        else return (a:d, Nothing )


createInit :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
createInit x = do
        a <- createToken
        (b, r) <- dataType
        if x then do
                s <- getState
                let name = "heap."++show (getHeapId s)
                updateState (symtableInsertInner (name, r, False))
                updateState increaseHeapId
                return (a:b, Just (Type.Ref (getTypeName r) name))
        else return (a:b, Nothing)


funcCall :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
funcCall x = do
        a <- idToken
        b <- beginExpressionToken
        (c,vs) <- args x
        d <- endExpressionToken
        if x then do
                s <- getState
                let (name, _, ts, inst) = getSubProg (getIdData a) s
                inp <- getInput
                setInput inst
                let oldScope = getProgramName s
                let oldCount = getCurrentScope s
                updateState (callFunc name)
                updateState (setCurrentScope 0)
                _ <- createVarsArgs ts vs
                (_, ret) <- subprogramStatements True
                updateState cleanVarsScope
                updateState (callFunc oldScope)
                updateState (setCurrentScope oldCount)
                setInput inp
                return (a:b:c++[d], ret)
        else return (a:b:c++[d],Nothing)

varCreation :: Bool -> ParsecT [Token] MyState IO [Token]
varCreation x = do
            (a, t) <- dataType
            b <- idToken
            (c, v) <- initialization x t
            if x then
                if not (compatible t (fromJust v)) then
                        error ("Não são compatíveis: " ++ show t ++ " - " ++ show (fromJust v))
                else do
                        -- o False abaixo é para constantes, no momento sem constantes
                        let var = (getIdData b, fromJust v, False)
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
                let (key, oldValue, constFlag) = symtableGetVar (getIdData a) s
                (b, st, ns, ov) <- dotAccess x (Just oldValue)
                if constFlag then error "Não se pode alterar uma constante"
                else do
                        (c, v) <- initialization x (fromJust ov)
                        if isRefType oldValue then do
                                if compatible (fromJust ov) (fromJust v) then
                                        do
                                        nv <- updateStructs (extractTypes st) ns (fromJust v) (fromJust v)
                                        let var = (key, oldValue, constFlag)
                                        updateState(symtableUpdate True nv var)
                                        return (a:b++c)
                                else if ifRefOf (fromJust ov) (fromJust v) then
                                        do
                                        nv <- updateStructs (extractTypes st) ns (fromJust v) (fromJust v)
                                        let var = (key, oldValue, False)
                                        updateState(symtableUpdate True nv var)
                                        return (a:b++c)
                                else error ("Não são compatíveis: " ++ show oldValue ++ " - " ++ show (fromJust v))
                        else do
                                if compatible (fromJust ov) (fromJust v) then
                                        do
                                        nv <- updateStructs (extractTypes st) ns (fromJust v) (fromJust v)
                                        let var = (key, nv, False)
                                        updateState(symtableUpdate False nv var)
                                        return (a:b++c)
                                else if ifRefOf (fromJust ov) (fromJust v) then
                                        do
                                        nv <- updateStructs (extractTypes st) ns (fromJust ov) (fromJust v)
                                        let var = (key, nv, False)
                                        updateState(symtableUpdate True nv var)
                                        return (a:b++c)
                                else error ("Não são compatíveis: " ++ show oldValue ++ " - " ++ show (fromJust v))
        else do
                (b, _, _, _) <- dotAccess x Nothing
                (c, _) <- initialization x (Type.Bool False)
                return (a:b++c)

updateStructs :: [Type] -> [String] -> Type -> Type-> ParsecT [Token] MyState IO Type
updateStructs [] [] _ v = do
        return v
updateStructs (st:ss) (name:ts) ref v = do
        nv <- updateStructs ss ts ref v
        let t = getStructField st name
        if isRefType t then do
                if isRefType nv then do
                        let a = updateStruct st (name,nv) nv
                        return a
                else do
                        let var = (getRefKey t, t, False)
                        updateState(symtableUpdate True nv var)
                        let a = updateStruct st (name,t) t
                        return a
        else do
                let a = updateStruct st (name,nv) nv
                return a
updateStructs a b c d = error ("debug: "++show a++" "++show b++" "++show c++" "++show d)


updateStruct :: Type -> (String, Type) -> Type -> Type
updateStruct (Type.Struct name trueParams) (pName, pValue) nValue
         = Type.Struct name (replaceArg trueParams (pName, pValue) nValue)
updateStruct _ _ _ = error "deu ruim"

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
        if x then do
                s <- getState
                let (_, oldValue, _) = symtableGetVar (getIdData b) s
                (c, _, _, ov) <- dotAccess x (Just oldValue)
                updateState (symtableRemoveRef (getRefKey (fromJust ov)))
                return (a:b:c)
        else do
                (c, _, _, _) <- dotAccess x Nothing
                return (a:b:c)

statements :: Bool -> ParsecT [Token] MyState IO [Token]
statements x = (do
        c <- statement x
        d <- semiColonToken
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
        <|> destroyCall x
        <|> try (procedureCall x)
        <|> try (varCreation x)
        <|> try (varAssignment x)
        <|> try (ifConditional x)
        <|> whileLoop x
        -- <|> try ()
        -- <|> try (continueToken x)
        -- <|> breakToken x

procedureCall ::Bool -> ParsecT [Token] MyState IO [Token]
procedureCall x = do
        a <- idToken
        b <- beginExpressionToken
        (c,vs) <- args x
        d <- endExpressionToken
        if x then do
                s <- getState
                let (name, _, ts, inst) = getSubProg (getIdData a) s
                inp <- getInput
                setInput inst
                let oldScope = getProgramName s
                let oldCount = getCurrentScope s
                updateState (callFunc name)
                updateState (setCurrentScope 0)
                _ <- createVarsArgs ts vs
                _ <- subprogramStatements True
                updateState cleanVarsScope
                updateState (callFunc oldScope)
                updateState (setCurrentScope oldCount)
                setInput inp
                return (a:b:c++[d])
        else return (a:b:c++[d])

createVarsArgs :: [(String, Type)] -> [Maybe Type] -> ParsecT [Token] MyState IO [Token]
createVarsArgs [] [] = return []
createVarsArgs [] _ = error "mais argumentos do que o necessário"
createVarsArgs _ [] = error "faltam argumentos"
createVarsArgs ((name,ty):ts) (v:vals) =
        if compatible ty (fromJust v) then do
                let var = (name, fromJust v, False)
                updateState(symtableInsert var)
                createVarsArgs ts vals
        else error "Tipos incompatíveis"


subprogramStatement :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
subprogramStatement x = do
        a <- statement x
        return (a,Nothing)

subprogramStatements :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
subprogramStatements x = (do
        (a,v1) <- subprogramStatement x <|> returnCall x
        if isReturnToken (head a) then do
                b <- semiColonToken
                (c,_) <- subprogramStatements False
                return (a++[b]++c, v1)
        else do
                b <- semiColonToken
                (c,ret) <- subprogramStatements x
                return (a++[b]++c, ret)
        ) <|> return ([],Nothing)

isReturnToken :: Token -> Bool
isReturnToken (Return _) = True
isReturnToken _ = False



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

readStatement :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
readStatement x = do
        a <- readToken
        if x then return ([a], Just (Type.String (unsafePerformIO getLine)))
        else return ([a], Nothing)


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



--example[20]
--example[20][5]
--Parser para obter o valor armazenado de alguma posicao em uma array
arrayAccess :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
arrayAccess execMode = do

                name <- idToken
                (rowIndex, rowIndexValue) <- subscript execMode
                (colIndex, colIndexValue) <- subscript execMode

                if execMode then do
                        s <- getState
                        let (_, lista, _) = symtableGetValue (getIdData name) s

                        if null colIndex then do
                                let result = evalArrayAcess lista (fromJust rowIndexValue)
                                return (name:rowIndex++colIndex, Just result)
                        else do
                                let result = evalMatrixAcess lista (fromJust rowIndexValue) (fromJust colIndexValue)
                                return (name:rowIndex++colIndex, Just result)
                else do
                        return (name:rowIndex++colIndex, Nothing)


-- lista;
-- lista[4];
--parser para alterar o valor de alguma posicao de uma lista
arrayModification :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
arrayModification execMode = do
                        name <- idToken
                        (rowIndex, rowIndexValue) <- subscript execMode
                        (colIndex, colIndexValue) <- subscript execMode
                        if execMode then do
                                s <- getState
                                let (key, lista, constFlag) = symtableGetValue (getIdData name) s
                                if null colIndex then do
                                        let defaultValue = evalArrayAcess lista (fromJust rowIndexValue)
                                        (initTok, newValue) <- initialization execMode defaultValue
                                        let result = evalArrayAssignment lista (fromJust rowIndexValue) (fromJust newValue)
                                        let var = (key, result, constFlag)
                                        updateState(symtableUpdate True result var)
                                        return (name:rowIndex++initTok, Just result)
                                else do
                                        let defaultValue = evalMatrixAcess lista (fromJust rowIndexValue) (fromJust colIndexValue)
                                        (initTok, newValue) <- initialization execMode defaultValue
                                        let result = evalMatrixAssignment lista (fromJust rowIndexValue) (fromJust colIndexValue) (fromJust newValue)
                                        let var = (key, result, constFlag)
                                        updateState(symtableUpdate True result var)
                                        return (name:rowIndex++colIndex++initTok, Just result)
                        else do
                                (initTok, _) <- initialization execMode (Type.Bool False)
                                return (name:rowIndex++colIndex++initTok, Nothing)



--int[10] a;            colIndex == []
--int[10][2] a;         colIndex != []
-- minhalista;
arrayCreation :: Bool -> ParsecT [Token] MyState IO [Token]
arrayCreation execMode = do
                        (tks, types) <- dataType
                        (rowIndex, rowValue) <- subscript execMode
                        if null rowIndex then error "faltando primeiro indice"
                        else do
                                (colIndex, colValue) <- subscript execMode
                                name <- idToken
                                if execMode then do
                                        if null colIndex then do
                                                let result = evalCreateArray (fromJust rowValue) types
                                                updateState(symtableInsert (getIdData name, result, False))       -- (identificador, lista, é constante)
                                                return (tks++rowIndex++[name])

                                        else do
                                                let result = evalCreateMatrix (fromJust rowValue) (fromJust colValue) types
                                                updateState(symtableInsert (getIdData name, result, False))       -- (identificador, lista, é constante)
                                                return (tks++rowIndex++colIndex++[name])

                                else do
                                        return (tks++rowIndex++colIndex++[name])

--parser para lidar com o operador []
subscript :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
subscript execMode = do
                        a <- beginListConstToken
                        (index, value) <- expression execMode
                        c <- endListConstToken
                        return (a:index++[c], value)
                        <|> return ([], Nothing)

