module Problems_lists where

import System.Random
import Control.Monad (replicateM)

--  Problem 21 - Insert an element at a given position into a list.
insertAt :: Int -> Char -> [Char] -> [Char]
insertAt a b ys@(x:xs) 
  | length ys < a = ys
  | a == 1 = b:ys 
  | otherwise = [x] ++ insertAt (a - 1) b xs

-- Problem 23 - Extract a given number of randomly selected elements from a list.
rndSel :: Int -> [a] -> IO [a]
rndSel 0 xs = return []
rndSel n xs = replicateM n (randomRIO (1, length xs) >>= (\idR -> return (xs !! idR)))

-- replicateM n x = sequence (replicate n x)
-- -- replicate  3 'a' ==> ['a','a','a']
-- -- sequence  type is ==> Monad m => [m a] -> m [a] ===> [Just 3, Just 3, Just 3] = Just [3,3,3]
-- En el caso de  rndSel replicate retorna [(IO a), (IO a), (IO a), ...] y replicate lo pasa a  (IO)[a,a,a,...]
                  
-- Lo mismo eliminando el elemento que escogemos de la lista
rndSelect2 :: [a] -> Int -> IO [a]
rndSelect2 _  0 = return []
rndSelect2 [] _ = return []
rndSelect2 xs count = do r <- randomRIO (0, (length xs)-1)
                         rest <- rndSelect2 (removeIt xs (r+1)) (count-1)
                         return ((xs!!r) : rest)

rndSelect3 :: [a] -> Int -> IO [a]
rndSelect3 _  0 = return []
rndSelect3 [] _ = return [] 
rndSelect3 xs count =  randomRIO (0, (length xs)-1) >>= (\r -> rndSelect3 (removeIt xs (r+1)) (count-1) >>=  (\rest -> return ((xs!!r) : rest)))

removeIt [] _ = []
removeIt ys@(x:xs) i
   | i > length ys = ys
   | i == 1 = xs
   | otherwise = [x] ++ (removeIt xs (i-1))