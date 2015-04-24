import System.Random
import Data.List
import Control.Monad

-- I don't really understand this code for random lists.
defaultGen = mkStdGen 11
randomList :: Int -> StdGen -> [Integer]
randomList n = take n . unfoldr (Just . random)

clean :: Eq a => a -> [a] -> [a]
clean x xs = filter (\a -> a /= x) $ nub xs

-- this is horners method for computing polynomials
coeficients2poly :: (Num a) => [a] -> a -> a
coeficients2poly cx x = foldl1 (\a b -> x*a + b) cx


msg2poly :: Int -> Integer -> Integer -> Integer
msg2poly degree message = coeficients2poly $ cs ++ [message]
  where cs = randomList degree defaultGen


-- Primary method to produce cryptographically shareable points from message
msg2shares :: Int -> Int -> Integer -> [(Integer, Integer)]
msg2shares k n m = map (\x -> (x, p x)) xs
  where
    p = msg2poly (k - 1) m
    xs = map fromIntegral [1..n]


shares2points :: [(Integer, Integer)] -> [(Rational, Rational)]
shares2points = map share2point
  where
    share2point (a, b) = (fromIntegral a, fromIntegral b)


main =
  let
    k = 4
    n = 10
    shares = msg2shares k n 1234
    message = points2message $ take k $ reverse $ shares2points shares
  in do
    print shares
    print message


points2message :: (Integral b, RealFrac a) => [(a, a)] -> b
points2message points = floor $ points2poly points 0


points2poly :: (Eq b, Fractional b) => [(b, b)] -> b -> b
points2poly points = \x -> foldr1 addFunctions ljs x
  where
    ljs = map (\p -> l p points) points

addFunctions :: (Monad m, Num b) => m b -> m b -> m b
addFunctions a b = liftM2 (+) a b


l :: (Eq a, Fractional a) => (a, a) -> [(a, a)] -> a -> a
l point ps = \x -> yVal * (l' j mx) x
  where
    yVal = snd point
    j:mx = (fst point) : (map fst $ clean point ps)


l' :: Fractional a => a -> [a] -> (a -> a)
l' j mx = foldr1 multFunctions $ map (l'' j) mx

multFunctions :: (Monad m, Num b) => m b -> m b -> m b
multFunctions a b = liftM2 (*) a b


l'' :: Fractional a => a -> a -> a -> a
l'' j m = \x -> (x - m)/(j - m)
