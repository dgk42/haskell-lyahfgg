import System.Environment
import Prime


main :: IO ()
main = do
  (num:_) <- getArgs
  let n = read num :: Int
    in putStrLn $ show $ take n primes
