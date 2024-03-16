import System.Random
import Control.Monad.State
-- A new alias for a list of Int, called Stack
type Stack = [Int]

-- The function takes a Stack and return a tuple of Int and a Stack
pop :: Stack -> (Int, Stack)
-- pattern match the Stack, which is a list, to separate the head from the tail
-- and return the head a the first element of the tuple and the tail as the 
-- second element
pop (x:xs) = (x,xs)

-- The function takes a Int and a Stack, and return a tuple with a dummy value 
-- and a Stack
push :: Int -> Stack -> ((), Stack)
-- pattern match to name the Int as 'a' and the Stack as 'xs'.
-- return a tuple where the first element is just a dummy value that doesn't matter,
-- and the second element is adding 'a' to the head of the list 'xs'.
push a xs = ((), a:xs)

-- Takes a initial Stack, and return a tuple of a Int and a new Stack
stackManip :: Stack -> (Int, Stack)
-- Names the Stack 'stack', and start using let expression
stackManip stack = let
-- calls the push function to add '3' to the stack and pass the 'stack' to it.
-- 'push' will return a dummy value and a new stack with the number '3' on top, we name
-- this new stack 'newStack1'.
  ((), newStack1) = push 3 stack
-- 'pop' will remove the first value from the stack and return it along side with a
-- new stack without it. We pass 'newStack1' that 'push' returned. 
  (a , newStack2) = pop newStack1
-- Call 'pop' again with the stack that the previous 'pop' returned.
  in pop newStack2
-- This function is a stateful computation, we are changing the state of the stack.

-- A wrapper of stateful computations, that comes with Control.Monad.State
-- It controls the state of a given stateful computation 's' and has a result of type 'a'
newtype State s a = State { runState :: s -> (a,s)}

-- Monad instance for stateful computations.
instance Monad (State s) where
-- Take a value and make a stateful computation that always has that value as the result.
-- That means the lambda will just present the value and the unchanged state.
-- We're just using function application '$' to apply the function to the State.
  return x = State $ \s -> (x,s)
-- Feeding a stateful computation to a function with '>>=' has to result in a 
-- stateful computation. That's why it starts with a State that that will have a 
-- lambda applied with the function application operator '$'.
-- We give the the stateful computation 'h' the current state 's', and that will
-- give a result and a new state.
  (State h) >>= f = State $ \s -> let (a, newState) = h s
-- We apply the result 'a' to the function 'f' to get the new state 'g'.
                                      (State g) = f a
-- So with a new stateful computation and a new state 'newState'. We apply
-- the stateful computation 'g' to 'newState' to get the final result
                                  in g newState
-- '>>=' kind of glue two stateful computations together. The second one is hidden
-- inside a function that takes the previous state result.


popMonad :: State Stack Int
-- Basically its wrapping the result '(x,xs)' to the State newtype
-- 'popMonad' is reliant of an initial State to extract from.
-- The type 'State' expect a function, that's why lambda is being used.
popMonad = State $ \(x:xs) -> (x,xs)

pushMonad :: Int -> State Stack ()
pushMonad a = State $ \xs -> ((),a:xs)

stackManipMonad :: State Stack Int
stackManipMonad = do
  pushMonad 3
  a <- popMonad
-- since we are not using 'a', we could just replace the line above for just 'popMonad'
  popMonad
-- the MonadState type class have two useful functions.
-- The 'get' function takes the current state and presents as the result.
get = State $ \s -> (s,s)

-- The 'put' takes a state and makes a stateful function that replaces the current state
-- with the new state
put newState = State $ \s -> ((), newState)

stackyStack :: State Stack ()
stackyStack = do
-- stackNow will be the current state that is passed to 'stackyStack'.
  stackNow <- get
  if stackNow == [1,2,3]
-- If the stack matches, replace the current state to '[8,3,1]'
      then put [8,3,1]
      else put [9,2,1]


randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- ramdonSt
  return (a,b,c)

-- runState threeCoins (mkStdGen 13)
-- ((True,False,True), 12312423145, 30249583245)
