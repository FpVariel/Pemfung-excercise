primes = sieve [2 ..]
    where sieve (x:xs) = x:[y | y <- xs, y `mod` x /= 0]