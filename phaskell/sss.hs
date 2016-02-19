import System.Random
import System.Random.Shuffle
import Data.List
import Control.Monad
import Math.NumberTheory.Prime


msg2ppoly :: Int -> Integer -> Integer -> (Integer, Integer, Integer)
msg2ppoly degree message =
  let
    defaultGen = mkStdGen 11

    cs = randomList degree defaultGen ++ [message] -- coeficients
    prime = nextPrime $ maximum cs
    horners cx x = foldl1 (\a b -> x*a + b) cx
  in
    \x -> (x, horners cs x `mod` prime, prime)


-- Primary method to produce cryptographically shareable points from message
msg2shares :: Int -> Int -> Integer -> [(Integer, Integer, Integer)]
msg2shares k n m = 
  let
    p = msg2ppoly (k - 1) m
    xs = [1..(fromIntegral n)]
  in
    map p xs


main =
  let
    msg = 1234
    k = 4
    n = 100
    shares = msg2shares k n msg

  in do
    allpoints <- shuffleM (shares2points shares)
    let msg = points2message $ take k allpoints
    let getprime (x, y, prime) = prime

    let prime = getprime $ shares!!0
    print prime
    print $ msg `mod` prime

integer2rational :: Integer -> Rational
integer2rational i = fromIntegral i

shares2points = map share2point
  where
    share2point (x, y, prime) = (integer2rational x, integer2rational y)


points2message :: (Integral b, RealFrac a) => [(a, a)] -> b
points2message points = floor $ points2poly points 0

points2poly :: (Eq b, Fractional b) => [(b, b)] -> b -> b
points2poly points = 
  let
    multFunctions a b = liftM2 (*) a b
    addFunctions  a b = liftM2 (+) a b

    ljs = map (\p -> l p points) points
    l point ps = 
      let

        yVal = snd point
        j:mx = (fst point) : (map fst $ clean point ps)
        l' j mx = foldr1 multFunctions $ map (l'' j) mx
        l'' j m = \x -> (x - m)/(j - m)
      in
        \x -> yVal * (l' j mx) x
  in
    \x -> foldr1 addFunctions ljs x

randomList :: Int -> StdGen -> [Integer]
randomList n = take n . unfoldr (Just . random)

clean :: Eq a => a -> [a] -> [a]
clean x xs = filter (\a -> a /= x) $ nub xs
