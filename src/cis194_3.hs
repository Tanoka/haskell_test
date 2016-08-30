module Golf where

import qualified Data.List as L

data IntList = Empty | Cons Integer IntList deriving Show
  
absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs) --abs está definido abs :: Num a => a -> a y funciona
  {-|
mapIntList ::  Num a => (a -> a) -> IntList -> IntList --da error. la funcion f, a pesar de estar definida igual que abs, no funciona...
mapIntList f Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

myFiler :: (Num a) => (a -> Bool) -> IntList -> IntList
myFiler f Empty = Empty
myFiler f (Cons x xs)
    | f x = Cons x (myFiler f xs)
    | otherwise = myFiler f xs
-}
data List t = E | C t (List t) deriving Show	 

myFiler' :: (t -> Bool) -> List t -> List t
myFiler' f E = E
myFiler' f (C x xs)
    | f x = C x (myFiler' f xs)
    | otherwise = myFiler' f xs
-- myFiler' (>2)  (C 3 (C 5 (C 2 E)))
	
mapIntList' :: (a -> b) -> List a -> List b 
mapIntList' f E = E
mapIntList' f (C x xs) = C (f x) (mapIntList' f xs)	
-- mapIntList' (+1)  (C 3 (C 5 (C 2 E)))

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- Exercices
skips :: [a] -> [[a]]
skips [] = []
skips xs= map  (\n -> (map fst (filter (isInSkip n) $ zip xs [1..]))) [1..(length xs)]
       where isInSkip n x = if ((snd x) `mod` n) == 0 then True else False
-- Para poder conocer la posicion de cada elemento de la lista hacemos zip con una lista infinita [1..], esto nos retorna una lista de tuplas en la que la primera posicion la ocupa el elemento de la lista a tratar y la segunda posicion de la tupla indica la posicion en la lista de ese elemento. De esta manera mediante Filter podemos saber si un elemento de la lista está en la posición que estamos tratando en ese momento, esto lo hace la funcion isInSkip. Para poder tratar todas las posiciones creamos una lista desde 1 al tamaño de la lista a tratar y por cada posición el proceso indicado anteriormente genera una lista con los resultados.
	   
skips' :: [a] -> [[a]]
skips' [] = []
skips' xs= map  (\n -> (map fst (filter ((==n) . snd) $ zip xs  (cycle [1..n])))) [1..(length xs)]
-- En esta version no hace falta utilizar `mod`, para ello el zip interno lo hacemos contra una lista que llega hasta el n que buscamos y despues se repite.  La construcción ((==n) . snd) 
-- putStrLn (show (1 + 1))  ----> putStrLn $ show $ 1 + 1  ---> putStrLn . show $ 1 + 1 --> pero  show $ 1 + 1 no puede ponerse un . porque (1 + 1) doesn't have an input, and therefore cannot be used with the . operator, . only chain functions
-- The compose operator (.) creates a new function without specifying the arguments
-- f = g . h -->  f x = (g . h) x --> f x = g (h x)
{-
putStrLn (show (1 + 1))
putStrLn (show $ 1 + 1)
putStrLn $ show (1 + 1)
putStrLn $ show $ 1 + 1
--Con punto
(putStrLn . show) (1 + 1)
putStrLn . show $ 1 + 1
-- putStrLn . show ( 1 + 1) Error
-- putStrLn . (show ( 1 + 1)) Error
-- putStrLn . show (1 + 1) Error
-}


--localMaxima [2,9,5,6,1] == [9,6]
--localMaxima [2,3,4,1,5] == [4]
--localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima (x:xy@(y:z:_)) = if (y>x && y>z) then y: localMaxima xy else localMaxima xy 
localMaxima _ = [] -- En lugar de poner caso a caso! localMaxima [] = [], localMaxima [x] = [] u localMaxima (x:y) = []   

histogram :: [Int] -> String
histogram xs = unlines ((reverse (L.transpose ((foldr (\n acc -> printlinea acc n) [(take 10 (cycle " "))]  xs)))) ++  ["0123456789"])
                 where printlinea lin n = ((take n (cycle "*")) ++ (take ((maximum xs) - n) (cycle " "))) : lin
 
-- take n (cycle "*") =====> replicate n '*' atencion, cycle utilizad comilla doble, porque replica el contenido de una lista. Replicate crea una lista a partir de un elemento, si ponemos una lista crea n listas.
 
contarNum :: [Int] -> [Int]
contarNum xs = foldr (\n acc -> funci acc n)  (take 10 (cycle [0])) xs
                 where funci xy x = take x xy ++ fundrop (drop x xy)
                       fundrop (y:xz) = (y + 1) : xz

histogram' :: [Int] -> String
histogram' xs = unlines (reverse (L.transpose ((foldr (\n acc -> printlinea acc n) [(take 10 (cycle " "))]  (contarNum xs)))) ++  ["0123456789"])
                 where printlinea lin n = ((take n (cycle "*")) ++ (take ((maximum (contarNum xs)) - n) (cycle " "))) : lin
                       contarNum :: [Int] -> [Int]
                       contarNum xs = foldr (\n acc -> funci acc n)  (take 10 (cycle [0])) xs
                         where funci xy x = take x xy ++ fundrop (drop x xy)
                               fundrop (y:xz) = (y + 1) : xz					   
					   