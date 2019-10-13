-- Excercise from chapter 9 & 10 --

-- Define the length function using map and sum.
myLength ls = sum(map (const 1) ls)


-- Give the type of, and define the function iter so that
-- iter n f x = f (f (... (f x))) 
-- where f occurs n times on the right-hand side of the equation.
-- For instance, we should have
-- iter 3 f x = f (f (f x)) 
-- and iter 0 f x should return x.
iter :: Int -> (Int -> Int) -> Int -> Int
iter 0 f x = x
iter n f x = f ( iter (n-1) f x)

-- How would you define the sum of the squares of the natural numbers 1
-- to n using map and foldr?