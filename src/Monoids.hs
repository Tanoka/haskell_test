{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monoids where


-- Si se quiere construir dos o mas monoides sobre un mismo tipo hay que usar newtype
{--  Por ejemplo, esto solo permitiria crear un monoide de a-" sobre la suma, pero no la multiplicacion
instance => Monoid a where
  mempty  = Sum 0
  mappend = (+)
--}

-- Newtype crea un "sinonimo" de "a" llamado "Sum a". En realidad es como hacer un nuevo "data", aunque esto nos obligaria a implementar los typeclass de Num
-- Monoide para la suma. 
newtype Sum a = Sum a  deriving (Eq, Ord, Num, Show) -- Para poder derivar de Num hay que poner {-# LANGUAGE GeneralizedNewtypeDeriving #-}

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where
  mempty  = Sum 0
  mappend = (+)
  
-- Monoide para el producto
newtype Product a = Product a deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a  

instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend = (*)
  
-- ejemplo de uso 
lst :: [Integer]
lst = [1,5,8,23,423,99]

prod :: Integer
prod = getProduct . mconcat . map Product $ lst  