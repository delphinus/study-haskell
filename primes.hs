import Control.Monad

primes :: [Integer]
primes = f [2..] where
  f (p : ns) = p : f [ n | n <- ns, n `mod` p /= 0 ]

filterPrimeM :: MonadPlus m => Integer -> m Integer
filterPrimeM n
  | n < 2 = mzero
  | and [ n `mod` x /= 0 | x <- [2..n-1] ] = return n
  | otherwise = mzero

searchPrime :: MonadPlus m => [Integer] -> m Integer
searchPrime = foldr (mplus . filterPrimeM) mzero
