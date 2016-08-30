
module Cis194_5 where

class Listable a where   -- creo un type class y defino una fución que deben cumplir los tipos que implemente listable
  toList :: a -> [Int]

 -- :t toList  ----> toList :: Listable a => a -> [Int]

instance Listable Int where
   toList x = [x]

instance Listable Bool where
  toList True  = [1]
  toList False = [0]


{-!
LOS DOS CASOS A CONTINUACION DAN ERROR.

-- NO FUNCIONA PORQUE [Int] ES LO MISMO QUE ([] Int), y cualquier cosa despues del type constructor tiene que ser un type variable
-- y no se pueden usar type alias para simular una type variable
SOLUCION: Use -XFlexibleInstances if you want to disable this.

instance Listable [Int] where -- uso de 'id'!!
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a) --Vamos a crear una instancia de un tio creado por nosotros
instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r
-}

data Tree a = Empty | Node a (Tree a) (Tree a)

{-!
-- Error, el problema es que toList tiene que retornar [Int] pero (Tree a) No garantiza que a sea Int.
-- y si tratamos de poner (Tree Int) da error, por lo que explicamos arriba.
instance  Listable (Tree a) where
  toList Empty        = []
  toList (Node x l r) = (toList l) ++ [x] ++ (toList r)
-}
instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y
  
-- Así si funciona TreeFix retorna Integer siempre, lo que ya no está parametrizado  
data TreeFix = Nada | Nodo Int TreeFix TreeFix
instance  Listable TreeFix where
  toList Nada        = []
  toList (Nodo x l r) = (toList l) ++ [x] ++ (toList r)
  
  



