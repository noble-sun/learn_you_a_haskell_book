import Data.Monoid
import Control.Monad.Writer

-- Writer Monad: values attached to other values that act sort of like logs of computations.
-- Takes a tuple of a value 'a' and a log String, a function that takes 'a' and return a tuple,
-- and return the result of the function
applyLog :: (a, String) -> (a -> (b,String)) -> (b,String)
-- Pattern match the tuple to 'x' and 'log', and the function to 'f'.
-- Use 'let' expression to patterm match the result of the function 'f x' into 'y' and 
-- 'newLog'. And return the value 'y' with the log history 'log ++ newLog'
applyLog (x,log) f = let (y,newLog) = f x in (y, log ++ newLog)
-- (3, "Smallish gang.") `appplyLog` isBigGang
-- (False, "Smallish gang.Compared gang size to 9")

-- Same function as the above, but now it has a constraint saying that the 
-- second value of the tuples need to be a monoid, so that at the end we can
-- use the 'mappend' function to add the logs together.
applyLogMonoid :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLogMonoid (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

-- An alias type 
type Food = String
-- Another alias type but uses the 'newtype' Sum Int. 'newtype' is just a wrapper an
-- existing type into another one, so 'Sum' is a different type than 'Int', but act
-- the same way, except 'Sum' has the properties of monoids, since it came from
-- importing 'Data.Monoid'
-- Sum is implemented like this: newtype Sum a = Sum { getSum :: a }
type Price = Sum Int

-- This function just takes a 'Food', and depending on what food it is, return an
-- accompanying drink with the value to that drink.
addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)
-- ("beans", Sum 10) `applyLog` addDrink
-- 'applyLog' will apply 'beans' to 'addDrink'. 'addDrink' will patterm match and 
-- return ("milk", Sum 25). 'applyLog' will then return "milk" with "Sum 10 `mappend` Sum 25",
-- which will result in 'Sum 35'

-- The Writer type is just a wrapper to a tuple of a value with a monoid type attached to it.
newtype Writer w a = Writer { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
-- 'return' needs to return the minimal default context where the value is still valid. So, we
-- use the monoid function 'mempty', which represents the identity monoid, where if that value
-- is used with 'mappend' with another value, the result is that another value.
-- 'mempty' is like "", Sum 0, empty bytestrings.
  return x = Writer (x, mempty)
-- The implementation of '>>=' is the same as the funciton 'applyLog', only we wrap and
-- unwrap with 'Writer' type.
  (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- Its using the getter function that comes along with the Writer type 'runWriter' to retrive the
-- tuple that Writer is wrapping. 
-- We are also saying to treat '3' as a Writer type. So when we use 'return' on it,
-- it will use the implementation of the instance for the Writer monad.
-- runWriter (return 3 :: Writer String Int)
-- (3,"")

-- This function just wraps a Int into the Writer type with a singleton list as 
-- the monoid.
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Go number: " ++ show x])

multWithLog :: Writer [String] Int
-- Use 'do' notation to have multiple monads together.
multWithLog = do
-- Binds the monad 'logNumber 3', which is Writer(3, ["Go number: 3"]), to 'a'.
  a <- logNumber 3
-- Binds the monad 'logNumber 5', which is Writer(3, ["Go number: 5"]), to 'b'.
  b <- logNumber 5
-- The 'tell' function is to add a monoid value without having a value attached to it.
  tell ["Gonna multiply ther two"]
-- return the multiplication of values 'a' and 'b'
  return (a*b)
-- Just reminding that when in a 'do' notation, each line would be the same as having
-- '>>=' at the end, so the singleton lists will be mappended together.
-- runWriter multWithLog
-- (15,["Got number: 3","Got number: 5","Gonna multiply these two"])

