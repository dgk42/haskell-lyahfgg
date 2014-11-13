module Prime(
  isPrime,
  primes) where


primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:ps) = p : sieve [n | n <- ps, n `mod` p /= 0]

isPrime :: Int -> Bool
isPrime n = let nn = abs n
                ps = takeWhile (<= nn) primes
            in if nn < 2 then False
            	         else nn == (head (reverse ps))
