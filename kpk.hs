myMin (x:xs) = foldl (min)  x xs
kpk a b = myMin [x | x <- [(max a b) .. a*b] , x `mod` a == 0 && x `mod` b == 0]