import System.Random
import System.Random.Shuffle
import Data.List
import Control.Monad
import Math.NumberTheory.Prime

-- code for random lists.
defaultGen = mkStdGen 11
randomList :: Integer -> StdGen -> [Integer]
randomList n = take (fromIntegral n) . unfoldr (Just . random)

clean :: Eq a => a -> [a] -> [a]
clean x xs = filter (\a -> a /= x) $ nub xs

-- this is horners method for computing polynomials
coeficients2poly :: (Num a) => [a] -> a -> a
coeficients2poly cx x = foldl1 (\a b -> x*a + b) cx


msg2ppoly :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
msg2ppoly degree message = \x -> (x, coeficients2poly cs x `mod` prime, prime)
  where
    cs = randomList degree defaultGen ++ [message]
    prime = nextPrime $ maximum cs

-- Primary method to produce cryptographically shareable points from message
msg2shares :: Int -> Int -> Integer -> [(Integer, Integer, Integer)]
msg2shares k n m = map p xs
  where
    p = msg2ppoly (fromIntegral (k - 1)) (fromIntegral m)
    xs = map fromIntegral [1..n]


main =
  let
    k = 4
    n = 10
    shares = msg2shares k n 1234
    points = take k $ shuffleM $ shares2points shares
    --message = points2message $ take k $ shuffleM $ shares2points shares
  in do
    print shares
    --print message

shares2points :: [(Integer, Integer, Integer)] -> [(Integer, Rational, Rational)]
shares2points = map share2point
  where
    share2point (a, b, c) = (a, fromIntegral b, fromIntegral c)


points2message :: (Integral b, RealFrac a) => [(a, a)] -> b
points2message points = floor $ points2poly points 0

points2poly :: (Eq b, Fractional b) => [(b, b)] -> b -> b
points2poly points = \x -> foldr1 addFunctions ljs x
  where
    ljs = map (\p -> l p points) points
    l point ps = 
      let
        yVal = snd point
        j:mx = (fst point) : (map fst $ clean point ps)
        l' j mx = foldr1 multFunctions $ map (l'' j) mx
        l'' j m = \x -> (x - m)/(j - m)
      in
        \x -> yVal * (l' j mx) x


addFunctions :: (Monad m, Num b) => m b -> m b -> m b
addFunctions a b = liftM2 (+) a b

multFunctions :: (Monad m, Num b) => m b -> m b -> m b
multFunctions a b = liftM2 (*) a b
