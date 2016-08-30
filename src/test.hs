doubleMe x = x + x
doubleDo x y = doubleMe x + doubleMe y

elIf x = if x < 10 
		then x
		else x * 2
		
tamano xs = sum[1 | _ <-xs]		

-- Cada grupo de corchetes crea una lista, si los anidamos creamos listas de listas

-- Las listas de número son tipo Integer, lo pasamos a Float
tf1 = [1,3..10] :: [Float]
tf2 xs = xs :: [Float]
{-

-- Diferencia entre:
cartas = [2,4,10,20]
--fromIntegral es porque la lista cartas contiene números del tipo Integer y take pide el parámetro en Int.
[ y | x <- cartas, y <- take (fromIntegral x) [1,2..]]
-- [1,2,1,2,3,4,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
-- Una lista única

[[ y | y <- take (fromIntegral x) [1,2..] ] | x <- cartas ]
-- [[1,2],[1,2,3,4],[1,2,3,4,5,6,7,8,9,10],[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]]
-- Una lista de listas.  Los valores son los mismos.
-----
-}

lcc1 = [ x * y | x <-[1,2,3], y <- [2,4,6]] -- [2,4,6,4,8,12,6,12,18]
lcc2 = [ [x * y | y<-[2,4,6]] | x <-[1,2,3]] -- [[2,4,6],[4,8,12],[6,12,18]]


lc1 = [ x * 2 | x <- [1,2..10]]
-- Es lo mismo que:

lc2 = map (2*) [1,2..10]

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

sumtorial' :: Integer -> Integer
sumtorial' 0 = 0
sumtorial' n
	| n == 1 = 1
	| otherwise = n + sumtorial (n-1)
	
myLast :: [a] -> a
myLast xs = xs !! ((length xs) - 1)	
	
myLast' :: [a] -> a
myLast' (x:[]) = x
myLast' (_:xs) = myLast' xs		

myLast'' :: [a] -> a
myLast'' xs = head (reverse xs)

myLast''' :: [a] -> a
myLast''' = head . reverse 

numEl :: [a] -> Int
numEl [] = 0
numEl (x:xs) = 1 + numEl xs

numEl' :: [a] -> Int
numEl' xs = snd (last (zip xs [1..]))
numEl2 xs = snd $ last $ zip xs [1..]
-- Wow!! fst pq se cambia el orden los parametros en zip!!.
numEl3 = fst . last . zip [1..]
numEl4 xs = foldl (\acc n -> acc + 1)  0 xs
numEl5 xs = foldr (\n acc -> acc + 1)  0 xs -- El acumulado cambia de posicion
numEl6 xs = sum (map (\_ -> 1) xs) -- al poner 1 sin ninguna operación sustituye el valor..


myRev :: [a] -> [a]
myRev [] = []
myRev [x] = [x]
myRev (x:xs) = myRev xs ++ [x]

myRev1 :: [a] -> [a]
myRev1 [] = []
myRev1 [x] = [x]
myRev1 (xs) = myRev (init xs) ++ [last xs]

myRev2 ::  [a] -> [a]
myRev2 xs = foldl (\acc n -> n:acc) [] xs

pali :: Eq a => [a] -> Bool -- Hay que indicar explicitamente que el tipo a implementará la clase de tipo Eq
pali [] = True
pali [x] = True
pali (x:y:[]) = x == y
pali (x:xs) = pali (init xs) && x == last xs
  
  