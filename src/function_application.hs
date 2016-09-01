g1 x = reverse x
g2 = reverse

f1 x = tail (reverse x)
-- f1' x = tail reverse x -- error tail aplicado a dos argmument. " " se aplica desde la izquierda. The function ‘tail’ is applied 
                       -- to two arguments, but its type ‘[a0] -> [a0]’ has only one
-- f1'' = tail reverse    -- error ‘reverse’ is applied to too few arguments In the first argument of ‘tail’, namely ‘reverse’

f3 x = tail $ reverse x
-- f3' = tail $ reverse  -- error ‘reverse’ is applied to too few arguments In the second argument of ‘($)’, namely ‘reverse’
-- f3'' x = tail reverse $ x  -- error The first argument of ($) takes one argument, but its type ‘[a0]’ has none

{--
    (.) :: (b -> c) -> (a -> b) -> a -> c  
    f . g = \x -> f (g x)  
 
   Function composition puede sustituir muchas veces a funciones anónimas, como (\x -> negate (abs x) por (negate . abs)
   Function composition es right-associative por lo que puede sustituir a los paréntesis 
   
   $ infixr 0 mínima precerencia, asociativo por la derecha, los paréntesis los pone desde la derecha.
      ($) is just a function which happens to apply functions, and functions are just values
   . infixr 9 máxima precerencia
   " " espacio tiene la máxima preferencia, y es asociativo por la izquierda. f1 a f2 b c == ((((f1 a) f2) b) c) regular function application which has the highest precedence
   
    --}
f4 = tail . reverse
f4' x = (tail . reverse) x -- hay que poner paréntesis porque " " tiene mayor precedencia que .
f4'' x = tail . reverse $ x -- $ tiene menor precedencia que . por lo que la composicion de funciones se puede construrir bien ($) :: (a -> b) -> a -> b 

-- f4''' x = tail . reverse x -- error ‘reverse’ is applied to too many arguments In the second argument of ‘(.)’, namely ‘reverse x’
                              --  como . tiene precerencia más baja que " ", que tiene la máxima, primero se ejecuta "reverse x", como el
                              -- resultado de esta operación esto no es una función sino una lista, no puede formar parte de la function composition,
                              -- pues esta exige dos funciones.
--f4'''' x = tail . (reverse x) -- error ‘reverse’ is applied to too many arguments In the second argument of ‘(.)’, namely ‘(reverse x)’

f5 = ((+) . (2*)) -- Pide x y aplica, lo que da otra función ((+) (2*x)) la cual pide otro parámetro (+) (2*x) y
f5' x y = (+) y (( 2 *) x) -- Es lo mismo que la función f5  
 