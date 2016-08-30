-- anonymous function, also known as a lambda abstraction
-- (\x -> x > 100) 4 
-- (>100)  4 -- is an operator section equivalente a (\x -> x > 100) 

-- Function composition
foo :: (b -> c) -> (a -> b) -> (a -> c) -- (b -> c) -> (a -> b) -> a -> c is the same because -> is naturally right-associative
foo f g = \x -> f (g x)
-- foo = . ==> f . g = f (g x)

--Currying and partial application
-- f x y = (f x) y
-- f x y z = ...  es f = \x -> (\y -> (\z -> ...)).

-- Partial application
foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

multThree :: (Num a) => a -> a -> a -> a  -- multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z   -- ((multThree 3) 5) 9 . (multThree 3) retorna una funcion que pide otro parametro, el 5, que retorna otra funcion que pide otro parametro. Esto se ve en la declaración de la función a -> (a -> (a -> a)).
multTwoWithNine = multThree 9   --Al aplicar parcialmente la funcion esta retorna otra funcion que necesita los dos paramtros que faltan, o sea la parte (a -> (a -> a)), lo que retorna cuando procesa el primer parametro, de la declaracion multThree :: (Num a) => a -> (a -> (a -> a))

-- partially applied  . Le faltan dos parametros pero funcioana, fun 3 [1,2,3,4]
fun = foldr (+)  -- fun simplemente "hereda" la declaración de foldr, por eso sabe los parametros que necesita
-- skell doesn’t make it easy to partially apply to an argument other than the first. The one exception is infix operators, which as we’ve seen, can be partially applied to either of their two arguments using an operator section

--A higher-order function is a function that takes other functions as arguments or returns a function as result. 

flip' :: (a -> b -> c) -> b -> a -> c  -- flip' :: (a -> b -> c) -> (b -> a -> c) es lo mismo leido de forma diferente, el primero dice que los parametros son una funcion y dos paramatros, la segunda forma dice que toma una función y retorna una funcion que toma dos parametros, es la versión curried.
flip' f y x = f x y  
flip'' f = \x y -> f y x 

