mergeSort [] = []
mergeSort [a] = [a]
mergeSort x = merge (mergeSort left) (mergeSort right)
    where
        (left,right) = half x


half x = (take lhx x, drop lhx x)
    where lhx = length x `div` 2

merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) = if x > y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)
