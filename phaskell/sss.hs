#!/usr/bin/runghc
-- file: sss.hs

import System.Random
import Data.List

coeficients2poly :: (Num a) => [a] -> a -> a
coeficients2poly cx = coeficients2value cx

-- this is horners method for computing polynomials
coeficients2value :: (Num a) => [a] -> a -> a
coeficients2value cx x = foldl1 (\a b -> x*a + b) cx

-- I don't really understand this code for random lists.
defaultGen = mkStdGen 11
randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . random)

type Share = (Int, Int)

msg2poly :: Int -> Int -> Int -> Int
msg2poly degree message = 
  let
    cs = randomList degree defaultGen
  in
    coeficients2poly $ cs ++ [message]

-- Primary method to produce cryptographically shareable points from message
msg2shares :: Int -> Int -> Int -> [Share]
msg2shares k n m =
  let
    p = msg2poly (k - 1) m
    xs = randomList n defaultGen
  in
    map (\x -> (x, p x)) xs


main =
  let
    points = msg2shares 4 7 1000
  in  putStrLn $ show points


getX :: Share -> Int
getX s = fst s

getY :: Share -> Int
getY s = snd s

clean :: Eq a => a -> [a] -> [a]
clean x xs = filter (\a -> a /= x) $ nub xs





{-
--l :: Share -> [Share] -> Int -> Int
l point ps =
  let
    yVal = getY point
    x:xs = (getX point) : (map getX $ clean point ps)
  in
    yVal * l' x xs

--l' :: Int -> [Int] -> Int -> Int 
l' j mx = foldr (*) $ map (\m -> l'' j m) mx

l'' :: Fractional a => a -> a -> a -> a
l'' j m x = (x - m)/(j - m)
-}
