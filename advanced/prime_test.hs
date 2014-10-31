import Test.QuickCheck
import Data.Set (difference, toList, fromList)
import Prime


-- http://adumont.fr/blog/sieve-of-erathostenes/

testPrime :: Integral a => a -> Bool
testPrime n
          | n <= 1 = False
          | n == 2 = True
          | otherwise = all (\y -> n `mod` y /= 0) pseudoPrimes where
              pseudoPrimes = (2:[3,5..(floor . sqrt . fromIntegral) n])


prop_prime :: Property
prop_prime = forAll (elements [1..10000])
                    (\n -> let ps = takeWhile (<= n) primes
                           in all testPrime ps)

prop_not_prime :: Property
prop_not_prime = forAll (elements [1..10000])
                        (\n -> let ps = takeWhile (<= n) primes
                                   noPrimes = toList $ difference (fromList [1..n]) (fromList ps)
                               in all (not . testPrime) noPrimes)


deepCheck :: Testable prop => prop -> IO ()
deepCheck p = quickCheckWith stdArgs { maxSuccess = 500 } p


test :: IO ()
test = do
  deepCheck prop_prime
  deepCheck prop_not_prime
