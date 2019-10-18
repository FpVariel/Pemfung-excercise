data Expr = C Float
            | Expr :+ Expr
            | Expr :- Expr
            | Expr :/ Expr
            | Expr :* Expr
            deriving Show

evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (a :+ b) = evaluate a + evaluate b
evaluate (a :- b) = evaluate a - evaluate b
evaluate (a :* b) = evaluate a * evaluate b
evaluate (a :/ b) = evaluate a / evaluate b

{- No 1 Latihan UTS -}
mapExpr :: (Float -> Float) -> Expr -> Expr
mapExpr f (C x) = C (f x)
mapExpr f (a :+ b) = (mapExpr f a) :+ (mapExpr f b)
mapExpr f (a :- b) = (mapExpr f a) :- (mapExpr f b)
mapExpr f (a :* b) = (mapExpr f a) :* (mapExpr f b)
mapExpr f (a :/ b) = (mapExpr f a) :/ (mapExpr f b)