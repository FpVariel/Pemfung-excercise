data Expr = C Float
            | V String
            | Let String Expr Expr
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
evaluate (Let v e0 e1) = evaluate (subst v e0 e1)

subst v0 e0 (V v1) = if (v0 == v1) then e0 else (V v1)
subst _ _ (C c) = (C c)
subst v0 e0 (e1 :+ e2) = subst v0 e0 e1 :+ subst v0 e0 e2
subst v0 e0 (e1 :- e2) = subst v0 e0 e1 :- subst v0 e0 e2
subst v0 e0 (e1 :* e2) = subst v0 e0 e1 :* subst v0 e0 e2
subst v0 e0 (e1 :/ e2) = subst v0 e0 e1 :/ subst v0 e0 e2
subst v0 e0 (Let v1 e1 e2) = Let v1 e1 (subst v0 e0 e2)

{- No 1 Latihan UTS -}

mapExpr :: (Float -> Float) -> Expr -> Expr
mapExpr f (C x) = C (f x)
mapExpr f (a :+ b) = (mapExpr f a) :+ (mapExpr f b)
mapExpr f (a :- b) = (mapExpr f a) :- (mapExpr f b)
mapExpr f (a :* b) = (mapExpr f a) :* (mapExpr f b)
mapExpr f (a :/ b) = (mapExpr f a) :/ (mapExpr f b)

{- No 2 Latihan UTS -}

hoEval :: (Expr -> Float) -> Expr -> Float
-- newEval (C x) = x
-- asume that f is 'evaluate'
hoEval f (a :+ b) = foldl (+) (f a) [(f b)]
hoEval f (a :- b) = foldl (-) (f a) [(f b)]
hoEval f (a :* b) = foldl (*) (f a) [(f b)]
hoEval f (a :/ b) = foldl (/) (f a) [(f b)]