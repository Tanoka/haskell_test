module Test where

-- mi propio type
data MiTipo = Mio1 | Mio2 | Mio0 deriving (Show)


-- Defino su comportamiento para el typclass Eq
instance Eq MiTipo where
    (==) Mio1 Mio2 = False
    (==) Mio2 Mio1 = False
    (==) _ Mio0 = True
    (==) Mio0 _ = True
    (==) Mio1 Mio1 = True
    (==) Mio2 Mio2 = True    
    
    
-- Creo una typeclass
class MiClass a where
    fun1 :: a -> a -> a
    fun2 :: a -> Integer
    
-- Defino el comportamiento de MiTipo en el typclass creado antes
instance MiClass MiTipo where
    fun1 Mio1 Mio2 = Mio0
    fun1 Mio2 Mio1 = Mio0    
    fun1 Mio0 a = a    
    fun1 a Mio0 = a        
    fun2 Mio0 = 0
    fun2 Mio1 = 1
    fun2 Mio2 = 2
    

---------- Lo mismo pero con type constructors
data MiTipoI a = Mio1I a | Mio2I a | Mio0I a deriving (Show)

instance Eq (MiTipoI a) where
    (==) (Mio1I _) (Mio2I _) = False
    (==) (Mio2I _) (Mio1I _) = False
    (==) (Mio1I _) (Mio0I _) = True
    (==) (Mio0I _) (Mio1I _) = True
    (==) (Mio1I _) (Mio1I _) = True
    (==) (Mio2I _) (Mio2I _) = True   
    
-- NO SE PUEDE HACER   -- instance MiClass (MiTipoI Int) where  a menos que pongamos {-# LANGUAGE FlexibleInstances #-}
instance (Integral a) => MiClass (MiTipoI a) where
    fun1 (Mio1I a) (Mio1I b) = Mio1I (a+b)
    fun1 (Mio2I a) (Mio2I b) = Mio2I (a+b)   
    fun1 (Mio0I _) (Mio0I _) = Mio2I 0
    fun2 (Mio0I _) = 0
    fun2 (Mio1I a) = toInteger(a)
    fun2 (Mio2I a) = toInteger(a)
    
    
-- Truco para hacer Instance de Maybe Int, Char, etc sin poner FlexibleInstances
------------------------------------------------------------------------------------------------
class Foo a where
  fnfo :: a -> a

instance  Foo Integer where
  fnfo a = a * 5

-- Si solo pongo esta instancia para los enteros peta, porque fnfo 5 desde GHCI el 5 es (Num) y no Int.
instance  Foo Int where
  fnfo a = a * 5  
  
instance Foo Char where
  fnfo a = a

instance Foo a=> Foo (Maybe a) where
  fnfo (Just a) = Just (fnfo a) 
    
    
------------------------------------------------------------------------------------------------
-- Typeclass
class Cls a where
 fn1 :: a -> a -> a

-- type
data Tpp = Tip | Top | Tup deriving (Show)

instance Cls Tpp where
 fn1 Tip _ = Tip
 fn1 Top _ = Top 
 fn1 Tup a = a 


-- type constructor
data Tcc a = Tic a | Toc a | Tuc a deriving (Show)

instance (Num a) => Cls (Tcc a) where
 fn1 t (Tuc _) = t
 fn1 (Tic a) (Tic b) = Toc (a + b)
 fn1 (Tic a) (Toc b) = Toc (a - b)
 fn1 (Toc a) (Tic b) = Toc ((-a) + b)
 fn1 (Toc a) (Toc b) = Toc ((-a) - b) 
 fn1 (Tuc _) t = t
 
-- Typeclass, f tiene que ser un type constructor
class Fcls f where
  fn2 :: (a -> b) -> f a -> f b
 

instance Fcls Tcc where
  fn2 fu (Tic a) = Tic (fu a)
  fn2 fu (Toc a) = Toc (fu a)
  fn2 fu (Tuc a) = Tuc (fu a)

instance Functor Tcc where
  fmap f (Tic a) = Tic (f a)
  fmap f (Toc a) = Toc (f a)
  fmap f (Tuc a) = Tuc (f a)

---  Functors
data Pair a = Pair a a deriving (Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
  
  
data MITree a = MLeaf Int | MNode [MITree Int] deriving (Show)  
mfn1 :: MITree Int -> Int
mfn1 (MLeaf a) = a
               
         
data NITree a = NLeaf a | NNode [NITree a] deriving (Show) 
nfn1 :: NITree a -> a
nfn1 (NLeaf a) = a
 

{--
 -- (Int -> a)  me indica que debo pasar una funcion a Leaf y cuando invoco la funcion tras el igual el primer parámetro debe ser Int.
data ITree a = Leaf (Int -> a)  | Node [ITree a]  deriving (Show) 
tfn1 ::ITree a -> Int -> a
tfn1 (Leaf f) n = f n


---  Functors
instance Functor ITree where
 fmap g (Leaf f) = Leaf (g . f)
--}



















    

    