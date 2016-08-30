-- Las funciones tienen tipo y podemos darles tipos explícitos al escribirlas.
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Las funciones no pueden empezar por mayuscula
doubleUs x y = x*2 + y*2
doubleUs2 x y = doubleMe x + doubleMe y 
-- da igual el orden en que se declaren las funciones, se pueden usar antes 
doubleMe x = x + x  

-- las sentencias if tienen que tener else obligatoriamente
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2 
						
-- When a function doesn't take any parameters, we usually say it's a definition (or a name)					
conanO'Brien = "It's a-me, Conan O'Brien!"						

-- In Haskell, lists are a homogenous data structure. It stores several elements of the same type, numbers, characters, lists...
lostNumbers = [4,8,15,16,23,42]  
-- inside GHCI ==> ghci> let lostNumbers = [4,8,15,16,23,42]  

--A common task is putting two lists together. This is done by using the ++ operator. 
-- [1,2,3,4] ++ [9,10,11,12] 
--  "hello" ++ " " ++ "world"
-- ['w','o'] ++ ['o','t'] 
-- : puts a list and a number or character together
-- 'A':" SMALL CAT"  
-- [1,2,3] is actually just syntactic sugar for 1:2:3:[]

-- If you want to get an element out of a list by index, use !!. The indices start at 0.
-- "Steve Buscemi" !! 6  
-- [9.4,33.2,96.2,11.2,23.25] !! 1  

-- [[],[],[]]  is a list that contains three empty lists.

-- Lists can be compared if the stuff they contain can be compared. When using <, <=, > and >= to compare lists,

-- Basic list functions: head =/= tail, last =/= init, length, null, reverse, take =/= drop,
-- maximum =/= minimum, sum, product, elem 

-- Ranges [1..20], ['K'..'Z'], [2,4..20], [20,19..1], Funciona de menor a mayor...No de mayor a menor
-- WATCH OUT => [0.1, 0.3 .. 1] ==> [0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]

--  the first 24 multiples of 13 [13,26..24*13], better way: take 24 [13,26..]

-- A handful of functions that produce infinite lists: cycle, repeat, replicate
-- take 12 (cycle "LOL ")  ==> "LOL LOL LOL "

-- list comprehension 
-- [x*2 | x <- [1..10]] => [2,4,6,8,10,12,14,16,18,20] 
-- [x*2 | x <- [1..10], x*2 >= 12] => [12,14,16,18,20]  
-- [ x | x <- [50..100], x `mod` 7 == 3] => [52,59,66,73,80,87,94] 

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   -- boomBangs [7..13] => ["BOOM!","BOOM!","BANG!","BANG!"]   

--Let's write our own version of length! We'll call it length'. _ means that we don't care what we'll draw from the list
length' xe = sum [1 | _ <- xe]  -- por cada elemento de la lista xs añade un 1 a la lista que retorna, esa lista la sumamos al final

-- [ [ x | x <- xs, even x ] | xs <- xxs]  xxs is a list of lists

-- Tuples
-- [(1,2),(8,11),(4,5)]  but [(1,2),(8,11,3),(4,5)] => error second element has 3 elements not 2
-- ("Christopher", "Walken", 55)
-- Two useful functions that operate on pairs: fst, snd, zip
-- Zip convierte dos listas en una lista de tuplas, cada tupla está formada por un elemento de cada lista de la misma posicion.
-- let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] 