-- Import a lib to deal with rational numbers
import Data.Ratio

-- A wrapper that is a list of tuples, with a value and a Rational.
-- The type derives from Show, so we can see the output in the standard output.
newtype Prob a = Prob { getProb :: [(a, Rational)]} deriving Show

-- Since Prob is actualy just a wrapper of a list, we can safely assume it is
-- also a Functor.
instance Functor Prob where
-- The implementation basically unwrapps the list of Prob with pattern match, by
-- naming 'xs'.
-- The lambda then pattern match the tuple and apply the function 'f' to the first
-- element of the tuple, and maintain the 'Rational' value. It then wraps the list
-- in the Prob type again.
  fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob
  [(Prob [('a',1%2),('b',1%2)], 1%4),
   (Prob [('c',1%2),('d',1%2)], 3%4)
  ]

flatten :: Prob (Prob a) -> Prob a
-- It will map multAll to the parameter 'xs' and use concat to flatten the resulting
-- list to then wrap it in Prob
flatten (Prob xs) = Prob $ concat $ map multAll xs
-- innerxs is a list of tuples, and 'p' is a Rational probability.
-- multAll will take this list and map over each tuple and apply the lambda.
-- The lambda just multiplies the Rational 'p' and the second element of the 
-- tuple, which is also a Rational value.
  where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Monad Prob where
-- 'return' is the same a 'pure', so its the minimal default context. For Prob, its 
-- a singleton list of a tuple where the first element is the value, and since its
-- a probability type, and its a single value, the probability is 1%1, or 100%.
  return x = Prob [(x,1%1)]
-- Basicaly apply the monadic value - which in this case is a Prob - to the function 
-- usgin fmap, and then flatten using the function declared above.
  m >>= f = flatten (fmap f m)
--  fail _ = Prob []

-- Creates a new type, where it can be Heads or Tails
data Coin = Heads | Tails deriving (Show, Eq)

-- The function is saying that both Heads and Tails have a 50-50 chance of happening
coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

-- This function says that Heads has a 10% chance and Tails has 90%
loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]


flipThree :: Prob Bool
-- Prob is a monad, so we can use the 'do' notation
flipThree = do
-- every line inside the 'do' notation needs to be a monad. It binds the result of
-- the function coin to 'a'
-- The 'do' notation is analogous to doing coin >>= coin >>= loadedCoin >>= return (all (==Tails) [a,b,c])
  a <- coin
  b <- coin
  c <- loadedCoin
-- check if all the elements of the list are equal to Tails, and return True if it is
  return (all (==Tails) [a,b,c])

-- getProb flipThree
--[(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),
--(False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]
