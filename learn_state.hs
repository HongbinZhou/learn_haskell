
-- ref: http://learnyouahaskell.com/for-a-few-monads-more#writer

-- <<< learn do notation
foo = Just "hi" >>= 
      (\x -> Just "!" >>= 
             (\y -> Just (show 2++ x ++ y)))  


foo' = do
  x <- Just "hi"
  y <- Just "!"
  Just (show 2 ++ x ++ y)

-- <<< learn State monad

type Stack = [Int]  
  
newtype State s a = State { runState :: s -> (a,s) }  
 
instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState  

myStack = [1,2,3]
--- <<<
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  

pop' :: State Stack Int  
pop' = State $ \(x:xs) -> (x,xs)  

test_pop = pop myStack
test_pop' = runState pop' myStack

--- <<<
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs) 
      
push' :: Int -> State Stack ()  
push' a = State $ \xs -> ((),a:xs)  

test_push = push 3 myStack
test_push' = runState (push' 3) myStack

--- <<<
stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
  ((),newStack1) = push 3 stack  
  (a ,newStack2) = pop newStack1  
  in pop newStack2  

-- use do nonation
stackManip' :: State Stack Int  
stackManip' = do  
    push' 3  
    pop'  
    pop'  

-- use >>=
stackManip'' :: State Stack Int
stackManip'' = (push' 3) >>= (\_ -> pop') >>= (\_ -> pop')
  
test_stackManip = stackManip myStack
test_stackManip' = runState stackManip' myStack
test_stackManip'' = runState stackManip'' myStack


