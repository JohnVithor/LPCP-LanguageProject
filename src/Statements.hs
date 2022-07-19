module Statements where
import Lexer
import Type
import Data.Maybe
import Text.Parsec
import Control.Monad.IO.Class
import SymTable
import TokenParser
import GHC.IO.Unsafe (unsafePerformIO)
import Eval


structDeclaration :: ParsecT [Token] MyState IO [Token]
structDeclaration = do
            b <- structToken
            c <- idToken
            d <- colonToken
            (e, fields) <- fieldCreations
            f <- endScopeToken
            g <- idToken
            if getIdData c /= getIdData g then error ("Nome da struct não é o mesmo: '"++getIdData c++"' != '"++getIdData g++"'")
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
            (a, t, constant) <- dataType
            b <- idToken
            if constant then error ("O campo '"++getIdData b++"' não pode ser constante pois campos de structs não devem ser imutáveis")
            else do 
                c <- semiColonToken 
                return (a++b:[c],(getIdData b, t))

dataType :: ParsecT [Token] MyState IO ([Token],Type, Bool)
dataType = constDataType <|> refDataType <|> try arrayCreation 
        <|> (do
        s <- getState
        t <- typeToken <|> idToken
        return ([t], typeTableGet t s, False)
        )

refDataType :: ParsecT [Token] MyState IO ([Token],Type, Bool)
refDataType = do
        r <- refToken 
        t <- typeToken <|> idToken
        return (r:[t], Type.Ref (getNameOfThat t) "", False)

constDataType :: ParsecT [Token] MyState IO ([Token],Type, Bool)
constDataType = do
        r <- constantToken 
        t <- typeToken <|> idToken
        s <- getState
        return (r:[t], typeTableGet t s, True)

getNameOfThat :: Token -> String 
getNameOfThat (Type _ x) = x
getNameOfThat (Id _ x) = x
getNameOfThat _ = error "Não é válido"


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
        (a, v) <- expression x <|> refInitialization x
        b <- commaToken
        (c, vs) <- args x
        return (a++b:c, v:vs))
        <|> do
        (a,v) <- expression x <|> refInitialization x
        return (a, [v])
        <|> return ([],[])

initialization :: Bool -> Type -> ParsecT [Token] MyState IO ([Token], Maybe Type)
initialization x t = (do
        c <- assignToken
        (d, r) <- refInitialization x <|> expression x <|> readStatement x <|> createInit x -- <|> structCreation x
        -- (d, r) <- refInitialization x <|> try (expression x)-- <|> try() <|> readStatement x <|> createInit x
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
        (b, r, constant) <- dataType
        if x then do
                s <- getState
                let name = "heap."++show (getHeapId s)
                updateState (symtableInsertInner (name, r, constant))
                updateState increaseHeapId
                return (a:b, Just (Type.Ref (getTypeName r) name))
        else return (a:b, Nothing)


varCreation :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
varCreation x = do
            (a, t, constant) <- dataType
            b <- idToken
            (c, v) <- initialization x t
            if x then do
                if not (compatible t (fromJust v)) then
                        error ("A declaração da variável '"
                         ++ getIdData b++"' não foi possivel ser realizada pois os tipos: '"
                         ++ show t ++ "' e '" ++ show (fromJust v) ++ "' não são compatíveis")
                else do
                        let coercedValue = coercion t (fromJust v)
                        let var = (getIdData b, coercedValue, constant)
                        updateState(symtableInsert var)
                        return (a++[b]++c, Nothing)
                else
                        return (a++[b]++c, Nothing)


varCreations :: Bool -> ParsecT [Token] MyState IO [Token]
varCreations x = (do
                    (c,_) <- varCreation x
                    e <- varCreations x
                    return (c++e))
                    <|> return []

varAssignment :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
varAssignment x = do
        a <- idToken
        if x then do
                s <- getState
                let (key, oldValue, constFlag) = symtableGetVar (getIdData a) s
                (b, st, ns, ov) <- dotAccess x (Just oldValue)
                if constFlag then error ("A variável '"++getIdData a++"' é uma constante e não se pode alterar uma constante")
                else do
                        (c, v) <- initialization x (fromJust ov)
                        if isRefType oldValue then do
                                if compatible (fromJust ov) (fromJust v) then do
                                        let coercedValue = coercion (fromJust ov) (fromJust v)                                        
                                        nv <- updateStructs (extractTypes st) ns coercedValue coercedValue
                                        -- liftIO(print "old")
                                        -- liftIO(print oldValue)
                                        -- liftIO(print "new")
                                        -- liftIO(print nv)
                                        let var = (key, oldValue, constFlag)
                                        -- liftIO (print "symtable")
                                        -- liftIO (print (getSymbolTbl s))
                                        updateState(symtableUpdate True nv var)
                                        return (a:b++c, Nothing)
                                else if isRefOf (fromJust ov) (fromJust v) then
                                        do
                                        let coercedValue = coercion (fromJust ov) (fromJust v)
                                        nv <- updateStructs (extractTypes st) ns coercedValue coercedValue
                                        let var = (key, oldValue, False)
                                        updateState(symtableUpdate True nv var)
                                        return (a:b++c,Nothing)
                                else error ("Não são compatíveis: " ++ show oldValue ++ " - " ++ show (fromJust v))
                        else do
                                if compatible (fromJust ov) (fromJust v) then
                                        do
                                        let coercedValue = coercion (fromJust ov) (fromJust v)
                                        nv <- updateStructs (extractTypes st) ns coercedValue coercedValue
                                        let var = (key, nv, False)
                                        updateState(symtableUpdate False nv var)
                                        return (a:b++c,Nothing)
                                else if isRefOf (fromJust ov) (fromJust v) then
                                        do
                                        let coercedValue = coercion (fromJust ov) (fromJust v)
                                        nv <- updateStructs (extractTypes st) ns coercedValue coercedValue
                                        let var = (key, nv, False)
                                        updateState(symtableUpdate True nv var)
                                        return (a:b++c,Nothing)
                                else error ("Não são compatíveis: " ++ show oldValue ++ " - " ++ show (fromJust v))
        else do
                (b, _, _, _) <- dotAccess x Nothing
                (c, _) <- initialization x (Type.Bool False)
                return (a:b++c,Nothing)

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
                        let var = ("", t, False)
                        updateState(symtableUpdate True nv var)
                        --let a = updateStruct st (name,t) nv
                        return st
        else do
                let a = updateStruct st (name,nv) nv
                -- liftIO(print "aqui?")
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
        (tk, tp) <- expression x <|> refInitialization x
        return (a:tk,tp)
        -- expression

destroyCall :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
destroyCall x = do
        a <- destroyToken
        b <- idToken
        if x then do
                s <- getState
                let (_, oldValue, _) = symtableGetVar (getIdData b) s
                (c, _, _, ov) <- dotAccess x (Just oldValue)
                updateState (symtableRemoveRef (getRefKey (fromJust ov)))
                return (a:b:c,Nothing)
        else do
                (c, _, _, _) <- dotAccess x Nothing
                return (a:b:c,Nothing)

statements :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
statements x = (do
                (a,v1) <- statement x
                b <- anyToken 
                if not (isSemiColon b) then error ("Falta um ';' após o: " ++ show (last a))
                else 
                        if x then do
                                if isJust v1 then do
                                        (c,_) <- statements False
                                        return (a++[b]++c, v1)
                                else do
                                        (c,ret) <- statements x
                                        return (a++[b]++c, ret)
                        else do
                                (c,_) <- statements x
                                return (a++[b]++c, Nothing)
                ) <|> return ([],Nothing)



isSemiColon :: Token -> Bool
isSemiColon (SemiColon _) = True
isSemiColon _ = False

statement :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type )
statement x =
        printStatement x
        <|> newLineStatement x
        <|> destroyCall x
        <|> try (ifConditional x)
        <|> try (procedureCall x)
        <|> try (arrayModification x)
        <|> try (varCreation x)
        <|> try (varAssignment x)
        <|> whileLoop x
        <|> returnCall x
        -- <|> try ()
        -- <|> try (continueToken x)
        -- <|> breakToken x

procedureCall ::Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
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
                updateState (callFunc (oldScope++"."++name))
                updateState (setCurrentScope 0)
                _ <- createVarsArgs ts vs
                _ <- statements True
                updateState cleanVarsScope
                updateState (callFunc oldScope)
                updateState (setCurrentScope oldCount)
                setInput inp
                return (a:b:c++[d],Nothing)
        else return (a:b:c++[d],Nothing)

createVarsArgs :: [(String, Type, Bool)] -> [Maybe Type] -> ParsecT [Token] MyState IO [Token]
createVarsArgs [] [] = return []
createVarsArgs [] _ = error "Foram informados mais argumentos do que o necessário"
createVarsArgs _ [] = error "Foram informados menos argumentos do que o necessário"
createVarsArgs ((name,ty, constant):ts) (v:vals) =
        if compatible ty (fromJust v) then do
                let var = (name, fromJust v, constant)
                updateState(symtableInsert var)
                createVarsArgs ts vals
        else error ("O Tipo '" ++ show ty ++ "' e o tipo '"++ show (fromJust v)++"' são incompatíveis")


isReturnToken :: Token -> Bool
isReturnToken (Return _) = True
isReturnToken _ = False

printStatement :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
printStatement x = do
        a <- printToken
        b <- beginExpressionToken
        (c, v) <- expression x
        d <- endExpressionToken
        if x then do
                liftIO (printVal (fromJust v))
                return (a:b:c++[d],Nothing)
        else return (a:b:c++[d],Nothing)

newLineStatement :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
newLineStatement x = do
        a <- newLineToken
        if x then do
                liftIO (putStrLn "")
                -- s <- getState 
                -- liftIO (print (getSymbolTbl s))
                return ([a],Nothing)
        else return ([a],Nothing)

{-# NOINLINE readStatement #-}
readStatement :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
readStatement x = do
        a <- readToken
        let v = unsafePerformIO getLine
        if x then return ([a], Just (Type.String v))
        else return ([a], Nothing)


-- <conditional> := begin if ( <logic_expression> ): <statements> end if
ifConditional :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
ifConditional x = do
                a <- beginScopeToken
                b <- ifToken
                c <- openParenthesesToken
                (d,v) <- logExpr x
                e <- closeParenthesesToken
                f <- colonToken
                if x then do
                    let r = getLogExprResult (fromJust v)
                    if r then do
                        updateState enterScope
                        (g,ret) <- statements True
                        updateState cleanVarsScope
                        updateState exitScope
                        h <- endScopeToken
                        i <- ifToken
                        (j,_) <- elseConditional False
                        return (a:b:c:d++e:f:g++h:i:j,ret)
                    else do
                        (g,_) <- statements False
                        h <- endScopeToken
                        i <- ifToken
                        (j,ret2) <- elseConditional True
                        return (a:b:c:d++e:f:g++h:i:j,ret2)
                else do
                    (g,_) <- statements x
                    h <- endScopeToken
                    i <- ifToken
                    (j,_) <- elseConditional x
                    return (a:b:c:d++e:f:g++h:i:j, Nothing)

elseConditional :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
elseConditional x = try (do
                a <- beginScopeToken
                b <- elseToken
                c <- colonToken
                if x then do
                        updateState enterScope
                        (d,ret) <- statements x
                        updateState cleanVarsScope
                        updateState exitScope
                        e <- endScopeToken
                        f <- elseToken
                        return (a:b:c:d++e:[f], ret)
                else do
                        (d,_) <- statements x
                        e <- endScopeToken
                        f <- elseToken
                        return (a:b:c:d++e:[f],Nothing)
                        )
                <|> do
                        return ([],Nothing)

-- begin while(logicExpression): stmts end while
whileLoop :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
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
                        (g,ret) <- statements r
                        updateState exitScope
                        h <- endScopeToken
                        i <- whileToken
                        -- checar retorno ret
                        if r then do
                                inp <- getInput
                                setInput (a:b:c:d++e:f:g++h:[i])
                                fr <- whileLoop x
                                setInput inp
                                return fr
                        else do
                                return (a:b:c:d++e:f:g++h:[i],ret)
                else do
                        (d,_) <- logExpr x
                        e <- closeParenthesesToken
                        f <- colonToken
                        (g,_) <- statements x
                        h <- endScopeToken
                        i <- whileToken
                        return (a:b:c:d++e:f:g++h:[i],Nothing)

expression :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
expression x = try (do
                (a,r) <- numExpr x
                return (a,r))
        <|> try (do
                (a,r) <- logExpr x
                return (a,r))
        <|> try (do
                (a,r) <- stringExpr x
                return (a,r))
        <|> ( do
                a <- castingToken
                (b, v) <- expression x
                if x then return (a:b, Just (cast a (fromJust v)))
                else return (a:b, Nothing)
        )

castingToken :: ParsecT [Token] u IO Token
castingToken = castingBoolToken
        <|> castingIntToken
        <|> castingRealToken
        <|> castingCharToken
        <|> castingStringToken

-- Parser inicial para expressões numéricas (soma e subtração)
numExpr :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
numExpr x = try (do
        (t1, n1) <- numTerm x
        (t2, n2) <- evalRemaining x n1
        if x then return (t1 ++ t2, n2)
        else return (t1 ++ t2, Nothing))
        <|> numTerm x

evalRemaining :: Bool -> Maybe Type -> ParsecT [Token] MyState IO ([Token],Maybe Type)
evalRemaining x n1 = try(do
                op <- plusToken <|> minusToken
                (t2, n2) <- numTerm x
                if x then do
                        let e = eval (fromJust n1) op (fromJust n2)
                        (t3, r) <- evalRemaining x (Just e)
                        return (op:t2++t3, r)
                else do
                        (t3, r) <- evalRemaining x Nothing
                        return (op:t2++t3, r))
                <|> return ([], n1)

-- Parser secundário para expressões numéricas (multiplicação, divisão e módulo)
numTerm :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
numTerm x = try (do
        (t1, n1) <- numFactor x
        op <- multToken <|> divToken <|> modToken
        (t2, n2) <- numTerm x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> numFactor x

-- Parser final para expressões numéricas (literais, variáveis, parênteses, etc)
numFactor :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
numFactor x = try (do
                (tk,tp) <- intToken <|> realToken
                return ([tk], Just tp)
                ) <|> try (getVar x False)
                <|> try ( do
                a <- minusToken
                (b, v) <- numFactor x
                if x then do return (a:b, Just (evalUni a (fromJust v)))
                else return (a:b, Nothing )
                ) <|> (do
                a <- beginExpressionToken
                (tk,tp) <- numExpr x
                c <- endExpressionToken
                if x then return ([a] ++ tk ++ [c], tp)
                else return ([a] ++ tk ++ [c],Nothing)
                )

-- Parser inicial para expressões lógicas (OR)
logExpr :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
logExpr x = try (do
        (t1, n1) <- logTerm1 x
        op <- orToken
        (t2, n2) <- logExpr x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> logTerm1 x

-- Parser secundário para expressões lógicas (AND)
logTerm1 :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
logTerm1 x = try (do
        (t1, n1) <- logTerm2 x
        op <- andToken
        (t2, n2) <- logTerm1 x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> logTerm2 x

-- Parser ternário para expressões lógicas (NOT)
logTerm2 :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
logTerm2 x = try (do
        op <- notToken
        (t1, n1) <- logFactor x
        if x then return (op : t1,Just (evalUni op (fromJust n1)))
        else return (op : t1, Nothing))
        <|> logFactor x

-- Parser final para expressões lógicas (booleano, variáveis, parênteses, etc) -
logFactor :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
logFactor x =   try (do
                (tk,tp) <- comparison x
                if x then return (tk, tp)
                else return (tk,Nothing)
                ) <|> try (do
                (tk,tp) <- boolToken
                return ([tk], Just tp)
                ) <|> try (getVar x False) <|> try (do
                a <- beginExpressionToken
                (tk,tp) <- logExpr x
                c <- endExpressionToken
                if x then return ([a] ++ tk ++ [c], tp)
                else return ([a] ++ tk ++ [c],Nothing)
                ) <|> do
                        b <- nullToken
                        (a,v) <- getVar x True
                        if x then do
                                return (b:a,Just (Type.Bool (isRefNull (fromJust v))))
                        else return (b:a,Nothing)

                -- IN
                -- IS

getVar :: Bool -> Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
getVar x y =  try (do
        (a, b) <- funcCall x
        return (a,b))
        <|> try (arrayAccess x) <|> (do
        a <- idToken
        s <- getState
        if x then do
                if y then do
                        let (_, oldValue, _) = symtableGetVar (getIdData a) s
                        (_, _, _, ov) <- dotAccess x (Just oldValue)
                        return ([a], ov)
                else do
                        let (_, oldValue, _) = symtableGetValue (getIdData a) s
                        (_, _, _, ov) <- dotAccess x (Just oldValue)
                        return ([a], ov)
        else do
                (b, _, _, _) <- dotAccess x Nothing
                return (a:b,Nothing)
        )

dotAccess :: Bool -> Maybe Type -> ParsecT [Token] MyState IO ([Token], [Maybe Type], [String] ,Maybe Type)
dotAccess x v = (do
        b <- dotToken
        c <- idToken
        if x then do
                let name = getIdData c
                let tv = fromJust v
                --liftIO(print "out")
                --liftIO(print tv)
                if isRefType tv then do
                        s <- getState
                        let (_,refV,_) = symtableGetInner2 (getRefKey tv) (getSymbolTbl s)
                        --liftIO(print "in")
                        --liftIO(print refV)
                        let fieldValue = getStructField refV name
                        (d, sts, ns, vv) <- dotAccess x (Just fieldValue)
                        return ([b,c]++d, Just refV:sts, name:ns, vv)
                else do
                        let fieldValue = getStructField tv name
                        (d, s, ns, vv) <- dotAccess x (Just fieldValue)
                        return ([b,c]++d, v:s, name:ns, vv)
        else do
                (d, s, ns, vv) <- dotAccess x v
                return ([b,c]++d, v:s, "":ns, vv)
        ) <|> do
                return ([],[],[],v)

-- Os operadores '==' e '!=' podem trabalhar com strings
-- Utilizei apenas numExpr nos trys desses dois operadores
--Falta implementar stringExpr
comparison :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
comparison x = do
                (t1, n1) <- try (numExpr x) <|> stringExpr x
                op <- comparisonOp
                (t2, n2) <- try (numExpr x) <|> stringExpr x
                if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
                else return (t1 ++ [op] ++ t2, Nothing)


comparisonOp :: ParsecT [Token] MyState IO Token
comparisonOp = equalToken
                <|> notEqualToken
                <|> lessOrEqualToken
                <|> greaterOrEqualToken
                <|> greaterToken
                <|> lessToken

stringExpr :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
stringExpr x = try (do
        (t1, n1) <- stringFactor x
        op <- plusToken
        (t2, n2) <- stringExpr x
        if x then return (t1 ++ [op] ++ t2,Just (eval (fromJust n1) op (fromJust n2)))
        else return (t1 ++ [op] ++ t2, Nothing))
        <|> stringFactor x

-- Expressão envolvendo strings ou chars ou id
-- TODO: verificar o tipo de idToken
stringFactor :: Bool -> ParsecT [Token] MyState IO ([Token],Maybe Type)
stringFactor x = try (do
                (tk, tp) <- stringToken <|> charToken
                return ([tk], Just tp))
               <|> getVar x False

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
                updateState (callFunc (oldScope++"."++name))
                updateState (setCurrentScope 0)
                _ <- createVarsArgs ts vs
                (_, ret) <- statements True
                updateState cleanVarsScope
                updateState (callFunc oldScope)
                updateState (setCurrentScope oldCount)
                setInput inp
                return (a:b:c++[d], ret)
        else return (a:b:c++[d],Nothing)

-- lista;
-- lista[4];
--parser para alterar o valor de alguma posicao de uma lista
arrayModification :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
arrayModification execMode = do
                        name <- idToken
                        a <- beginListConstToken
                        (rowIndex, rowIndexValue) <- expression execMode
                        c <- endListConstToken
                        (colIndex, colIndexValue) <- subscript execMode
                        if execMode then do
                                s <- getState
                                let (key, lista, constFlag) = symtableGetValue (getIdData name) s
                                if null colIndex then do
                                        let defaultValue = evalArrayAcess lista (fromJust rowIndexValue)
                                        (initTok, newValue) <- initialization execMode defaultValue
                                        if compatible defaultValue (fromJust newValue) then do
                                                let result = evalArrayAssignment lista (fromJust rowIndexValue) (fromJust newValue)
                                                let var = (key, result, constFlag)
                                                updateState(symtableUpdate False result var)
                                                return (name:a:rowIndex++c:initTok, Nothing)
                                        else error ("Não são compatíveis: " ++ show defaultValue ++ " - " ++ show (fromJust newValue))

                                else do
                                        let defaultValue = evalMatrixAcess lista (fromJust rowIndexValue) (fromJust colIndexValue)
                                        (initTok, newValue) <- initialization execMode defaultValue
                                        if compatible defaultValue (fromJust newValue) then do
                                                let result = evalMatrixAssignment lista (fromJust rowIndexValue) (fromJust colIndexValue) (fromJust newValue)
                                                let var = (key, result, constFlag)
                                                updateState(symtableUpdate False result var)
                                                return (name:a:rowIndex++c:colIndex++initTok, Nothing)
                                        else  error ("Não são compatíveis: " ++ show defaultValue ++ " - " ++ show (fromJust newValue))
                        else do
                                (initTok, _) <- initialization execMode (Type.Bool False)
                                return (name:a:rowIndex++c:colIndex++initTok, Nothing)



--int[10] a;            colIndex == []
--int[10][2] a;         colIndex != []
-- minhalista;
arrayCreation :: ParsecT [Token] MyState IO ([Token],Type, Bool)
arrayCreation = do
                t <- typeToken <|> idToken
                s <- getState
                let (tks, types) = ([t], typeTableGet t s)
                a <- beginListConstToken
                (rowIndex, rowValue) <- intToken
                c <- endListConstToken
                (colIndex, colValue) <- getCol
                if null colIndex then do
                        let result = evalCreateArray types rowValue
                        return (tks++[a,rowIndex,c],result, False)
                else do
                        let result = evalCreateMatrix types rowValue (fromJust colValue) 
                        return (tks++[a,rowIndex,c]++colIndex,result, False)


getCol :: ParsecT [Token] MyState IO ([Token], Maybe Type)
getCol = do
        a <- beginListConstToken
        (colIndex, colValue) <- intToken 
        c <- endListConstToken
        return (a:colIndex:[c], Just colValue)
        <|> return ([],Nothing)


--example[20]
--example[20][5]
--Parser para obter o valor armazenado de alguma posicao em uma array
arrayAccess :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
arrayAccess execMode = do
                name <- idToken
                a <- beginListConstToken
                (rowIndex, rowIndexValue) <- expression execMode
                c <- endListConstToken
                (colIndex, colIndexValue) <- subscript execMode
                if execMode then do
                        s <- getState
                        let (_, lista, _) = symtableGetValue (getIdData name) s
                        if null colIndex then do
                                let result = evalArrayAcess lista (fromJust rowIndexValue)
                                return (name:a:rowIndex++c:colIndex, Just result)
                        else do
                                let result = evalMatrixAcess lista (fromJust rowIndexValue) (fromJust colIndexValue)
                                return (name:a:rowIndex++c:colIndex, Just result)
                else do
                        return (name:a:rowIndex++c:colIndex, Nothing)

--parser para lidar com o operador []
subscript :: Bool -> ParsecT [Token] MyState IO ([Token], Maybe Type)
subscript execMode = do
                        a <- beginListConstToken
                        (index, value) <- expression execMode
                        c <- endListConstToken
                        return (a:index++[c], value)
                        <|> return ([], Nothing)