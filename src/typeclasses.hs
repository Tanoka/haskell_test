
-- Haskell has type inference ( If we write a number, we don't have to tell Haskell it's a number. It can infer that on its own)
--  :t examine the types of some expressions in GHCI 
-- :t "HELLO!" => "HELLO!" :: [Char] -> read as => has type of a list of characters.
-- Types are written in capital case

-- Functions also have types. When writing our own functions, we can choose to give them an explicit type declaration.
removeNonUppercase :: [Char] -> [Char]  -- string to string. Sames as > removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 

addThree :: Int -> Int -> Int -> Int  -- The return type is the last item in the declaration and the parameters are the first three
addThree x y z = x + y + z

-- Commonn types: Int, Integer, Float, Double , Bool, Char

-- Type variables
-- ghci> :t head => head :: [a] -> a ==> It takes a list of any type and returns one element of that type
-- ghci> :t fst => fst :: (a, b) -> a  ==> fst takes a tuple which contains two types and returns an element which is of the same type as the pair's first component.

--Typeclasses
-- Note: the equality operator, == is a function. So are +, *, -, / and pretty much all operators. If a function is comprised only of special characters, it's considered an infix function by default. If we want to examine its type, pass it to another function or call it as a prefix function, we have to surround it in parentheses.

-- ghci> :t (==)  
-- (==) :: (Eq a) => a -> a -> Bool  
-- Everything before the => symbol is called a class constraint
-- The equality function takes any two values that are of the same type and returns a Bool
-- The type of those two values must be a member of the Eq class (this was the class constraint).
-- The Eq typeclass provides an interface for testing for equality.
-- The 'elem' function has a type of (Eq a) => a -> [a] -> Bool because it uses == over a list to check whether some value we're looking for is in it.

-- basic typeclasses: 
-- Eq - is used for types that support equality testing. The functions its members implement are == and /=. So if there's an Eq class constraint for a type variable in a function, it uses == or /= somewhere inside its definition
-- Ord - is for types that have an ordering. Ord covers all the standard comparing functions such as >, <, >= and <= . To be a member of Ord, a type must first have membership in the prestigious and exclusive Eq club.
-- Show - Members can be presented as strings. All types covered so far except for functions are a part of Show
-- Read - is sort of the opposite typeclass of Show. The read function takes a string and returns a type which is a member of Read.
-- Enum - members are sequentially ordered types
-- Bounded - members have an upper and a lower bound.
-- Num - is a numeric typeclass. Its members have the property of being able to act like numbers

-- A very useful function for dealing with numbers is fromIntegral. 
-- fromIntegral :: (Num b, Integral a) => a -> b. It takes an integral number and turns it into a more general number
-- Notice that fromIntegral has several class constraints in its type signature. That's completely valid and as you can see, the class constraints are separated by commas inside the parentheses.

