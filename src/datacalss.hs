data Forma = Rectangulo Int Int | Circulo Int deriving (Show)

class Areas a where
  area :: a -> Float
  
instance Areas Forma where 
  area (Rectangulo a l) = fromIntegral  (a * l)
  area (Circulo r) = (fromIntegral 2) * 3.14 * (fromIntegral r)
  

data Buzon a = Carta a Int | Paquete a deriving (Show)

{-- 
-- Esto peta (Couldn't match expected type ‘a’ with actual type ‘Forma’. ‘a’ is a rigid type variable bound by the instance declaration)
instance Areas a=>  Areas (Buzon a) where 
 area (Paquete (Rectangulo t l)) = fromIntegral  (t * l)
 --}
 -- Esto si funciona, utilizamos a, pero le decimos que debe ser de la clase Areas, de la cual ya tenemos 
-- definida una instancia para los parametros de tipo Forma     ç
-- Hay un ejemplo de tipos.hs de la clase Foo para el type constructor Maybe   
instance Areas a=> Areas (Buzon a) where           
 area (Paquete r) = area r
 area (Carta r t) = (area r) * (fromIntegral t)


