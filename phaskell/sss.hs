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

--points2message :: [Share] -> Int
points2message points = points2value points 0

--points2value :: [Share] -> Fractional -> Num
points2value points = \x -> foldr (+) $ map (\p -> l p points x) points

l point ps =
  let
    yVal = snd point
    j:mx = (fst point) : (map fst $ clean point ps)
  in
    yVal * (l' j mx)


--l' :: Fractional a -> [a] -> (a -> a)

l' j mx = foldr1 (*) $ map (\m -> l'' j m) mx

--l'' :: Fractional a => a -> a -> a -> a
l'' j m = \x -> (x - m)/(j - m)

clean :: Eq a => a -> [a] -> [a]
clean x xs = filter (\a -> a /= x) $ nub xs

addStuff = do
  a <- (*2)
  b <- (+10)
  return (a*b)
  
f c = do
  a <- (*c)
  b <- (+c)
  return $ a + b
-- \x -> foldl f x [1, 2, 3, 4, 5] :: Num b => b -> b
-- (*2) >>= \a -> ...

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
