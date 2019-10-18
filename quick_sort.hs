quickSort :: (Enum a) => a -> a
quickSort [] = []
quickSort (x:xs) = quickSort[a | a <- xs, a > x] ++ [x] ++ quickSort[a | a <- xs, a < x]