
main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return () 
        
{--
-- >> then operator
putStr "Hello" >> 
putStr " " >> 
putStr "world!" >> 
putStr "\n" 

es lo mismo que

do putStr "Hello"
   putStr " "
   putStr "world!"
   putStr "\n"
   
   

do x1 <- action1
   x2 <- action2
   action3 x1 x2

es lo mismo que

-- >>= bind operator  
action1 >>= \ x1 -> action2 >>= \ x2 -> action3 x1 x2

o

action1
  >>=
    \ x1 -> action2
      >>=
        \ x2 -> action3 x1 x2

-- El programa inicial con el bind y then operator
main =   
    getChar >>=
      \ c ->  if c /= ' '  then 
            putChar c   >> 
            main  
        else return () 
--}
        