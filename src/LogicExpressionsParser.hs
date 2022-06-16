

orToken = tokenPrim show updatePos get_token where
  get_token (Or p) = Just (Or p)
  get_token _      = Nothing


andToken = tokenPrim show updatePos get_token where
  get_token (And p) = Just (And p)
  get_token _      = Nothing

notToken = tokenPrim show updatePos get_token where
  get_token (Not p) = Just (Not p)
  get_token _      = Nothing

equalToken = tokenPrim show updatePos get_token where
  get_token (Equal p) = Just (Equal p)
  get_token _      = Nothing

lessOrEqualToken = tokenPrim show updatePos get_token where
  get_token (LessOrEqual p) = Just (LessOrEqual p)
  get_token _      = Nothing

greaterOrEqualToken = tokenPrim show updatePos get_token where
  get_token (GreaterOrEqual p) = Just (GreaterOrEqual p)
  get_token _      = Nothing

notEqualToken = tokenPrim show updatePos get_token where
  get_token (NotEqual p) = Just (NotEqual p)
  get_token _      = Nothing

lessToken = tokenPrim show updatePos get_token where
  get_token (Less p) = Just (Less p)
  get_token _      = Nothing

greaterToken = tokenPrim show updatePos get_token where
  get_token (Greater p) = Just (Greater p)
  get_token _      = Nothing

inToken = tokenPrim show updatePos get_token where
  get_token (In p) = Just (In p)
  get_token _      = Nothing

isToken = tokenPrim show updatePos get_token where
  get_token (Is p) = Just (Is p)
  get_token _      = Nothing

--a or b
orExp :: ParsecT [Token] [(Token,Token)] IO [Token]
orExp = do
            a <- logicExp
            b <- orToken
            c <- logicExp
            return (a:b:[c])


--a and b
andExp :: ParsecT [Token] [(Token,Token)] IO [Token]
andExp = do
            a <- logicExp
            b <- andToken
            c <- logicExp
            return (a:b:[c])

--not a
andExp :: ParsecT [Token] [(Token,Token)] IO [Token]
andExp = do
            a <- notToken
            b <- logicExp
            return (a:[b])

-- a==b <|> a!=b <|> a<b <|> a>b <|> a>=b <|> a<=b
comparisson :: ParsecT [Token] [(Token,Token)] IO [Token]
comparisson = do
            a <- expression
            b <- equalToken <|> notEqualToken <|> lessToken <|> greaterToken <|> greaterOrEqualToken <|> lessOrEqualToken
            c <- expression
            return (a:b:[c])

parentesisLogicExp :: ParsecT [Token] [(Token,Token)] IO [Token]
parentesisLogicExp = do
            a <- BeginExpression
            b <- inToken
            c <- EndExpression
            return (a:b:[c])

--a in lista
inclusion :: ParsecT [Token] [(Token,Token)] IO [Token]
inclusion = do
            a <- expression
            b <- inToken
            c <- idToken
            return (a:b:[c])

-- b is int
typeCheck :: ParsecT [Token] [(Token,Token)] IO [Token]
typeCheck = do
            a <- expression
            b <- isToken
            c <- typeToken
            return (a:b:[c])

logicExp :: ParsecT [Token] [(Token,Token)] IO [Token]
logicExp = do
            a <- (boolToken <|> idToken <|> notToken <|> andToken <|> orToken <|> ComparissonToken <|> parentesisLogicExp <|> inclusion <|> typeCheck)
            return (a)