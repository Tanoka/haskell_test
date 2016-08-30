module DoNotation where

-- (>>) (then) operator
imprime =
 putStr "Hello" >> 
 putStr " " >> 
 putStr "world!" >> 
 putStr "\n"

imprimeDo =  
  do putStr "Hello"
     putStr " "
     putStr "world!"
     putStr "\n"
   
-- (>>=) (bind) operator

-- action1 >>= \x1 -> action2 >>= \x2 -> action3 x1 x2
test1 = [1,2,3] >>= (\x -> [x+2]) >>= (\x -> [x/2])

{-- los actions tiene que ser tipos datatypes constructor o funciones en un datatype constructor, que implementen la clase monad.
do x1 <- action1
   x2 <- action2
   action3 x1 x2
--}
testDo =
  do x1 <- [1,2,3]  
     x2 <- [x1+2]
     [x2/2]   -- o más general, return (x2/2).
 
-- ejemplo de IO
leer = getLine >>= (\x -> 
                        (getLine >>= \y -> 
                                          putStrLn ("Has escrito: " ++ x ++ " y " ++ y)))

leerDo = 
  do line1 <- getLine
     line2 <- getLine
     putStrLn ("Has escrito: " ++ line1 ++ " y " ++ line2)
 