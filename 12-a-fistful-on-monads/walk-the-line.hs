
-- A alias or a synonym type, that just 'gives' another name for 'Int' to keep 
-- more on the context of the business rules.
type Birds = Int
-- A synonym type that says that a 'Pole' is a tuple of 'Int' values.
type Pole = (Birds, Birds)

-- These are simple functions that 'land' birds on each of the sides of the pole.
-- It takes as parameters 'Birds' (which is an alias for 'Int'), and 'Pole' (which
-- is an alias for a tuple of Ints), and will return an updated number of birds
-- on the pole. It returns a 'Pole' wrapped in a Maybe to treat for errors.
landLeft :: Birds -> Pole -> Maybe Pole
-- It gives name to the parameters using pattern matching.
landLeft n (left, right)
-- Uses guards to check if the difference of birds on each side is less than 4,
-- and if it is, return the added birds to the correct side wrapped with 'Just'.
  | abs ((left + n) - right) < 4 = Just (left + n, right)
-- If that difference is greater than 4, it returns the 'Nothing' value.
  | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing
-- With the ways the functions are structured, returning a 'Maybe Pole', it would be possible
-- to chain the functions with '>>=' ( (>>=) :: m a -> (a -> m b) -> m b ), which 
-- basically extracts the values from 'Maybe' and apply it to a function.
-- landRight 1 (0,0) >> landLeft 2
-- (2,1)

-- This is a function that makes it more readable when chaining functions by passing 
-- the parameters first and the function after.
-- 'x' would be the parameter, '-:' is a arbitrary symbol used to make chaining 
-- possible (I think?), and 'f' is the function.
x -: f = f x
-- So instead of this: landLeft 2 (landRight 1 (landLeft 1 (0,0))), it would be
-- possible to do it like this: (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2
-- The function is actually a partial applied function, because 'x' can only 
-- represent one of the parameters.

-- This is just a function that no matter what is passed, it will return 'Nothing',
-- just to make the chain of functions fail on the given context.
banana :: Pole -> Maybe Pole
banana _ = Nothing
-- return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
-- Instead of creating a new function just to return this predetermined result,
-- we could use '>>', that has this implementation:
-- (>>) :: (Monad m) => m a -> m b -> m b
-- m >> n = m >>= \_ -> n
-- 
-- 'Nothing >> Just 3' return 'Nothing' because of the implementation of '>>=' for
-- the Monadic Maybe, where if the left value is a 'Nothing', it returns 'Nothing'.
-- 'Just 3 >> Nothing' return 'Nothing' because '>>=' will apply '3' to a lambda 
-- that will return the right value.

-- So we can do something like this
-- return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1

