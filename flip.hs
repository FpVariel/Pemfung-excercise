mystery :: Num x => x -> x -> x
mystery a b = 2*a + b

myFlip :: (x -> y -> z) -> y -> x -> z
myFlip f = (\x y -> f y x)