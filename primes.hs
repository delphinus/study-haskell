primes :: [Integer]
primes = f [2..] where
  f (p : ns) = p : f [ n | n <- ns, n `mod` p /= 0 ]
