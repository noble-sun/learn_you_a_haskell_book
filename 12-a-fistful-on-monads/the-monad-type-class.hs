
class Monad m where
-- Works just like the pure function for Applicative Functors, its wraps something
-- in a monad
  return :: a -> m a
-- It reads as 'bind'. It's just like a function application, but it takes a monadic value
-- and a function that returns a monadic value.
  (>>=) :: m a -> (a -> m b) -> m b
-- Apparently no one ever uses it
  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y
-- It is used by Haskell for a special syntactic construct for monads.
  fail :: String -> m a
  fail msg = error msg


-- Implementation of Maybe instance of a Monad
instance Monad Maybe where
-- Its just pure, so it returns the value wrapped in a Just.
  return x = Just x
-- If the left value for 'bind' is 'Nothing', we wouldn't be able to apply it to
-- the function on the right, so it just return 'Nothing'.
  Nothing >>= f = Nothing
-- It takes the value wrapped and apply the function to it.
  Just x >>= f = f x
  fail _ = Nothing

-- return "WHAT" :: Maybe String
-- Just "WHAT"
--
-- Just 9 >>= \x -> return (x*10)
-- Just 90

