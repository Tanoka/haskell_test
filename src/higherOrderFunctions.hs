-- Curried functions
-- Every function in Haskell officially only takes one parameter.
-- All the functions that accepted several parameters so far have been curried functions. 
-- Putting a space between two things is simply function application. The space is sort of like an operator and it has the highest precedence
-- multThree :: (Num a) => a -> a -> a -> a  or multThree :: (Num a) => a -> (a -> (a -> a))
-- multThree x y z = x * y * z  
-- multThree 3 5 9. First, 3 is applied to multThree, because they're separated by a space. That creates a function that takes one parameter and returns a function. So then 5 is applied to that, which creates a function that will take a parameter and multiply it by 15. 9 is applied to that function and the result is 135 or something

-- partial application.
-- If we call a function with too few parameters, we get back a partially applied function, meaning a function that takes as many parameters as we left out. Using partial application (calling functions with too few parameters, if you will) is a neat way to create functions on the fly so we can pass them to another function or to seed them with some data.

-- Infix functions can also be partially applied by using sections. To section an infix function, simply surround it with parentheses and only supply a parameter on one side
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 
-- same as
divideByTen' :: (Floating a) => a -> a  
divideByTen' x = x/10

-- Functions can take functions as parameters and also return functions.
applyTwice :: (a -> a) -> a -> a   -- The first parameter is a function (of type a -> a) and the second is that same a.
applyTwice f x = f (f x)  
-- Parentheses here are mandatory. They indicate that the first parameter is a function that takes something and returns that same thing
-- applyTwice (+3) 10 ---> 16
-- applyTwice (++ " HAHA") "HEY" ---> "HEY HAHA HAHA" 
-- applyTwice ("HAHA " ++) "HEY"  ---> "HAHA HAHA HEY"  

-- zipWith. It takes a function and two lists as parameters and then joins the two lists by applying the function between corresponding elements. Here's how we'll implement it
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

-- Maps and filters
-- map takes a function and a list and applies that function to every element in the list, producing a new list. Let's see what its type signature is and how it's defined.
map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs
-- map (+3) [1,5,3,1,6] is the same as writing [x+3 | x <- [1,5,3,1,6]]
-- filter is a function that takes a predicate (a predicate is a function that tells whether something is true or not, so in our case, a function that returns a boolean value) and a list and then returns the list of elements that satisfy the predicate. The type signature and implementation go like this:
filter :: (a -> Bool) -> [a] -> [a]  
filter _ [] = []  
filter p (x:xs)   

-- another version of quicksort (in recursion.hs)
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
	let smallerSorted = quicksort (filter (<=x) xs)  
		biggerSorted = quicksort (filter (>x) xs)   
	in  smallerSorted ++ [x] ++ biggerSorted  
	| p x       = x : filter p xs  
	| otherwise = filter p xs  

--  we're going to find the sum of all odd squares that are smaller than 10,000.
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))   same as:
-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)]) 


-- Lambdas
-- Lambdas are basically anonymous functions that are used because we need some functions only once. Normally, we make a lambda with the sole purpose of passing it to a higher-order function. To make a lambda, we write a \ and then we write the parameters, separated by spaces. After that comes a -> and then the function body. Lambdas are expressions, that's why we can just pass them like that.
numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
	where isLong xs = length xs > 15  
-- with lambdas:
numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100])) 

-- map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)] 
 
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f x y = f y x
-- same as:
flipLambda f = \x y -> f y x  

--folds: A fold takes a binary function, a starting value (I like to call it the accumulator) and a list to fold up. The binary function itself takes two parameters. The binary function is called with the accumulator and the first (or last) element and produces a new accumulator. Then, the binary function is called again with the new accumulator and the now new first (or last) element, and so on. Once we've walked over the whole list, only the accumulator remains, which is what we've reduced the list to.
-- foldl function, also called the left fold.
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  
	
-- same as: The lambda function (\acc x -> acc + x) is the same as (+). We can omit the xs as the parameter because calling foldl (+) 0 will return a function that takes a list. Generally, if you have a function like foo a = bar b a, you can rewrite it as foo = bar b, because of currying.
sum' :: (Num a) => [a] -> a  
sum' = foldl (+) 0  	

--foldr works in a similar way to the left fold, only the accumulator eats up the values from the right.
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs  
-- with foldl
map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

--The foldl1 and foldr1 functions work much like foldl and foldr, only you don't need to provide them with an explicit starting value. They assume the first (or last) element of the list to be the starting value and then start the fold with the element next to it.
-- a bunch of standard library functions by using folds
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  

-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list. There are also scanl1 and scanr1, which are analogous to foldl1 and foldr1.

-- Function application with $. 
-- $ function, also called function application
-- the $ function has the lowest precedence. Function application with a space is left-associative (so f a b c is the same as ((f a) b) c)), function application with $ is right-associative.

-- sum (map sqrt [1..130]) same as: sum $ map sqrt [1..130]

--Function composition (f·g)(x)=f(g(x))
--  We do function composition with the . function, which is defined like so:
(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)  

map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
-- Using function composition
map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  

map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  
-- Using function composition
map (negate . sum . tail) [[1..5],[3..6],[1..7]] 

fn x = ceiling (negate (tan (cos (max 50 x))))  
-- Using function composition
fn = ceiling . negate . tan . cos . max 50  

oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))   
-- Using function composition
oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  

