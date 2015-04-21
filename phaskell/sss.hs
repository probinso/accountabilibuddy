import System.Random
import Data.List
import Control.Monad
import Debug.Trace
import Text.Printf


-- this is horners method for computing polynomials
coeficients2poly :: (Num a) => [a] -> a -> a
coeficients2poly cx x = foldl1 (\a b -> x*a + b) cx

-- I don't really understand this code for random lists.
defaultGen = mkStdGen 11
randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)

msg2poly :: Int -> Int -> Int -> Int
msg2poly degree message =
  let
    cs = randomList degree defaultGen
    poly = coeficients2poly $ cs ++ [message]
    dbg x = printf "f(%d) = %d\n" x $ poly x
  in
    trace (dbg 0) poly

    
-- Primary method to produce cryptographically shareable points from message
msg2shares :: Int -> Int -> Int -> [(Int, Int)]
msg2shares k n m =
  let
    p = msg2poly (k - 1) m
    xs = [1..n]
  in
    map (\x -> (x, p x)) xs


shares2points :: [(Int, Int)] -> [(Rational, Rational)]
shares2points = map share2point
  where
    -- share2point :: (Int, Int) -> (Rational, Rational)
    share2point (a, b) = (fromIntegral a, fromIntegral b)


main =
  let
    points = shares2points $ msg2shares 3 5 1000
    message = points2message points
  in do
    putStrLn $ show points
    putStrLn $ show message



points2message :: (Eq b, Fractional b) => [(b, b)] -> b
points2message points = points2poly points 0


points2poly :: (Eq b, Fractional b) => [(b, b)] -> b -> b
points2poly points =
  let
    ljs = map (\p -> l p points) points
  in
    \x -> foldr1 addFunctions ljs x

addFunctions :: (Monad m, Num b) => m b -> m b -> m b
addFunctions a b = liftM2 (+) a b


l :: (Eq a, Fractional a) => (a, a) -> [(a, a)] -> a -> a
l point ps =
  let
    yVal = snd point
    j:mx = (fst point) : (map fst $ clean point ps)
  in
    \x -> yVal * (l' j mx) x


l' :: Fractional a => a -> [a] -> (a -> a)
l' j mx = foldr1 multFunctions $ map (l'' j) mx

multFunctions :: (Monad m, Num b) => m b -> m b -> m b
multFunctions a b = liftM2 (*) a b


l'' :: Fractional a => a -> a -> a -> a
l'' j m = \x -> (x - m)/(j - m)


clean :: Eq a => a -> [a] -> [a]
clean x xs = filter (\a -> a /= x) $ nub xs


