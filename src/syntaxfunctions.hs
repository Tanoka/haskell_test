-- Pattern matching

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" 

-- factorial => product [1..n] or a function recursively:
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

-- it not uses pattern matching.
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors a b = (fst a + fst b, snd a + snd b) 
-- it uses pattern matching.
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  

-- fst, snd, our own third
third :: (a, b, c) -> c  
third (_, _, z) = z  

-- The x:xs pattern is used a lot, especially with recursive functions. But patterns that have : in them only match against lists of length 1 or more.

-- our own implementation of the head function
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  


tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y 

sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs  

-- The pattern xs@(x:y:ys), this pattern will match exactly the same thing as x:y:ys but you can easily get the whole list via xs instead of repeating yourself by typing out x:y:ys in the function body again. 
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  
    
-- Guards ==> (|)
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    
    
bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"      
    
-- Our own max function
max' :: (Ord a) => a -> a -> a  
max' a b | a > b = a | otherwise = b

--  3 `myCompare` 2  ==> GT
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT      
    
-- Note: Not only can we call functions as infix with backticks (`), we can also define them using backticks. Sometimes it's easier to read that way.    

bmiTell'' :: (RealFloat a) => a -> a -> String  
bmiTell'' weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
    

-- We put the keyword where after the guards (usually it's best to indent it as much as the pipes are indented) and then we define several names or functions. These names are visible across the guards and give us the advantage of not having to repeat ourselves   
bmiTell1 :: (RealFloat a) => a -> a -> String  
bmiTell1 weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5
          normal = 25.0
          fat = 30.0    

-- where bindings aren't shared across function bodies of different patterns. If you want several patterns of one function to access some shared name, you have to define it globally.          

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2

-- Let binding    
-- Let bindings let you bind to variables anywhere and are expressions themselves, but are very local, so they don't span across guards    
-- The form is let <bindings> in <expression>. The names that you define in the let part are accessible to the expression after the in part
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
    
-- The difference is that -let bindings- are expressions themselves. -where bindings- are just syntactic constructs.    

-- They can also be used to introduce functions in a local scope:
-- ghci> [let square x = x * x in (square 5, square 3, square 2)]   ===> [(25,9,4)]  

--If we want to bind to several variables inline, we obviously can't align them at columns. That's why we can separate them with semicolons.
-- ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar) ===>   (6000000,"Hey there!")  

-- You can also put let bindings inside list comprehensions.
calcBmis3 :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis3 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  

calcBmis4 :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis4 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0] 
-- We omitted the in part of the let binding when we used them in list comprehensions because the visibility of the names is already predefined there
--  The in part can also be omitted when defining functions and constants directly in GHCi. If we do that, then the names will be visible throughout the entire interactive session.

-- Case expressions

head2 :: [a] -> a  
head2 [] = error "No head for empty lists!"  
head2 (x:_) = x  
--Same as:
head2' :: [a] -> a  
head2' xs = case xs of [] -> error "No head for empty lists!"  
                       (x:_) -> x  
                      
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."      
describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  
                                               