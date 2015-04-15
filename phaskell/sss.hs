{- file: sss.hs
 - ----------------------------------------------------------------------------
 - "THE BEER-WARE LICENSE" (Revision 42):
 - <pmoss.robinson@gmail.com> wrote this file.  As long as you retain this 
 - notice you can do whatever you want with this stuff. If we meet some day, 
 - and you think this stuff is worth it, you can buy me a beer in return.
 -   Philip Robinson
 - 
 - 
 - ----------------------------------------------------------------------------
 -}

import System.Random
import Data.List
import Control.Monad
{-
coeficients2poly :: (Num a) => [a] -> a -> a
coeficients2poly cx = coeficients2value cx

-- this is horners method for computing polynomials
coeficients2value :: (Num a) => [a] -> a -> a
coeficients2value cx x = foldl1 (\a b -> x*a + b) cx

-- I don't really understand this code for random lists.
defaultGen = mkStdGen 11
randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)

msg2poly :: Int -> Int -> Int -> Int
msg2poly degree message =
  let
    cs = randomList degree defaultGen
  in
    coeficients2poly $ cs ++ [message]

-- Primary method to produce cryptographically shareable points from message
msg2shares :: Int -> Int -> Int -> [(Int, Int)]
msg2shares k n m =
  let
    p = msg2poly (k - 1) m
    xs = randomList n defaultGen
  in
    map (\x -> (x, p x)) xs


main =
  let
    points = msg2shares 4 7 1000
    message = points2message points
  in  putStrLn $ show message
-}

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
{-
addFunctions a b = do
  x <- a
  y <- b
  return (x+y)
-}

l :: (Eq a, Fractional a) => (a, a) -> [(a, a)] -> a -> a
l point ps =
  let
    yVal = snd point
    j:mx = (fst point) : (map fst $ clean point ps)
  in
    \x -> yVal * (l' j mx) x


l' :: Fractional a => a -> [a] -> (a -> a)
l' j mx = foldr1 multFunctions $ map (\m -> l'' j m) mx

multFunctions :: (Monad m, Num b) => m b -> m b -> m b
multFunctions a b = liftM2 (+*) a b


l'' :: Fractional a => a -> a -> a -> a
l'' j m = \x -> (x - m)/(j - m)


clean :: Eq a => a -> [a] -> [a]
clean x xs = filter (\a -> a /= x) $ nub xs


