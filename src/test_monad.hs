
-- Functor
class Functor f where  
	fmap :: (a -> b) -> f a -> f b  

-- map :: (a -> b) -> [a] -> [b]
instance Functor [] where  
	fmap = map  
	
instance Functor [] where
  fmap _ []     = []
  fmap f (x:xs) = f x : fmap f xs	
  
-- Ejemplos:  
-- map (*2) [1,2,3]  	
-- map (/3) $  map (+2) $ map (*2) [1,2,3] --Encadenando funcines sobre una lista
	
-- Applicative	
	
class (Functor f) => Applicative f where  
	pure :: a -> f a  
	(<*>) :: f (a -> b) -> f a -> f b  
	
instance Applicative [] where
  pure a        = [a]          -- a "deterministic" value
  [] <*> _      = []
  (f:fs) <*> xs = (map f xs) ++ (fs <*> xs)
		
instance Applicative [] where  
	pure x = [x]  
	fs <*> xs = [f x | f <- fs, x <- xs]  

-- Ejemplos:	
-- [(+3)] <*> [1,2]
-- pure (+) <*> [1,2] <*> [3,4]  
--[(+),(*)] <*> [1,2] <*> [3,4]  -- Encadenando	listas hasta completar los parámetros de las funciones de la lista.
	
-- Monad 	
class  Monad m  where
    (>>=)            :: m a -> (a -> m b) -> m b
    (>>)             :: m a -> m b -> m b
    return           :: a -> m a
    fail             :: String -> m a
    m >> k           =  m >>= \_ -> k -- podría ser simplement = k ..pero entonces no se ejecutaría m


-- concat :: [[a]] -> [a]		
instance Monad [] where  
	return x = [x]  
	xs >>= f = concat (map f xs) 
	fail _ = []  

instance Monad [] where  
	return x = [x]  
	l >>= f = concatMap f l
	fail _ = []  	
	
-- Otras Implementaciones de >>=	
--xs >>= f = concat $ [f] <*> xs -- Utilizando applicative para []
-- xs >>= f = join $ pure f <*> xs -- Utilizando applicative, general, ¿ Si está applicative definido este monad sirve para todos??
--
-- join :: Monad m => m (m x) -> m .. versión general de "concat"
-- xs >>= k = join (fmap k xs)	-- utilizando functor. tambien sería general, y f de monad no tendría porque ser applicative, solo functor.??

-- Ejemplos:
-- [1,2,3] >>= (\x -> [x+2])
-- [1,2,3] >>= (\x -> [x+2]) >>= (\x -> [x/2]) -- Encadenando funciones a una lista. o (\x -> return (x/2))

