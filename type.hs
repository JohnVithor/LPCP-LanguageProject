
data Type = Bool Bool | 
            Int Int |
            Real Double | 
            Char Char | 
            String [Char] | 
            Struct String [(String, Type)]
            deriving (Eq,Show)

typeTable :: [Type]
typeTable = [Bool False, Int 0, Real 0.0, Char ' ', String ""]

-- escopo, nome, tipo(+valor)
memory :: [(String, String, Type)]
memory = [ ]