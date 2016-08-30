-- CIS 194 Homework 4

module Cis194Exercise4 where

import Data.List

--Exercise 1
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs
  
fun1' :: [Integer] -> Integer
fun1' xs = foldl (\acc x -> acc * (x-2)) 1 (filter (even) xs)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

--Exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

insertTree :: (Show a, Eq a) =>  a -> Tree a -> Tree a
insertTree y h 
   | h == Leaf = (Node 0 Leaf y Leaf)  
insertTree y (Node n l x r) 
   | prof l == prof r = Node m (insertTree y l) x r   
   | prof l < prof r = Node n (insertTree y l) x r   
   | prof l > prof r = Node n l x (insertTree y r)
     where prof Leaf = - 1
           prof (Node z _ _ _) = z
           m = 1 + max (prof r) (prof (insertTree y l)) 
  
foldTree :: (Show a, Eq a) => [a] -> Tree a
foldTree xs = foldr (\x acc -> insertTree x acc) Leaf xs

-- Exercise 3
xor :: [Bool] -> Bool
xor xs = foldr (\x acc -> if (acc /= x) then True else False) False xs
   
xor' = foldr (/=) False   

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) [] 

-- Exercise 4

descartados :: Integer -> [Integer]
descartados n =  foldr  (\x acc -> (descarta x) ++ acc) [] [1..n]
             where  descarta :: Integer -> [Integer] 
                    descarta i = [ eliminado i j | j <-[i..n], eliminado i j < n]
                        where eliminado i j =  i+j+(2*i*j)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> (2*x+1)) (filter (\x -> not (elem x (descartados n))) [1..n])

-- sustituye la función anónima por function composition y partial aplication
sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map ((+1).(*2)) (filter (\x -> not (elem x (descartados n))) [1..n])

--  WOW WOW!!  \\ List-dierence operator
sieveSundaram'' :: Integer -> [Integer]
sieveSundaram'' n =  map ((+1).(*2)) ([1..n] \\ sieve)
                   where sieve = takeWhile (<=n) [ i+j+(2*i*j) | i <-[1..n], j <-[i..n]]

