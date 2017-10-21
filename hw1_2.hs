-- 1) GCD
import Prelude hiding(gcd)
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (mod a b)



-- test
main :: IO ()
main = do
  let res1 = gcd 9 6
  print res1
