
-- Implementation of Monad instance for lists
instance Monad [] where
-- wraps the value in a list
  return x = [x]
-- Will apply the function for every element of the list using 'map'. Then it just
-- flatten the result, which is a list of lists, using the 'concat'.
  xs >>= f = concat (map f xs)
-- If the monad results in a failure, it will return a empty list when the 'fail'
-- function is called
   fail _ = []

-- Translating this expression in 'do' notation:
-- [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
listOfTuples :: [(Int, Char)]
listOfTuples = do
-- Here its taking every monadic value and treating it like a normal value, and
-- because we're in a 'do' the context of non-determinism is preserved with the
-- implicit '>>='.
  n <- [1,2]
  ch <- ['a', 'b']
  return (n, ch)

-- MonadPlus is a typeclass for monads that can also act as monoids.
class Monad m => MonadPlus m where
-- 'mzero' is the equivalent to 'mempty' for the monoid. So it is the identity, which
-- is the minimun value that when used with another parameters, the result will always
-- be the other parameter. Like '[] ++ [1,2]', the identity is []
  mzero :: m a
-- 'mplus' is the equivalent to 'mappend' for the monoid. So its just a function
-- that takes two parameters of the same type and return a third value that has
-- that type.
  mplus :: ma -> m a -> m a

-- Since a list is a monoid and also a monad, we can make it an instance of 'MonadPlus'
instance MonadPlus [] where
  mzero = []
  mplus = (++)


guard :: (MonadPlus m) => Bool -> m ()
-- If its True, it will put '()' in a minimal context that still succeeds for the Monad.
guard True = return ()
-- If its False, it will return the failed monadic value for that context.
guard False = mzero

-- guard (5 > 2) :: [()]
-- [()]
-- guard (1 > 2) :: Maybe ()
-- Nothing

