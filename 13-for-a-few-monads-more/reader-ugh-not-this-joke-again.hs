-- instance implementation of Monad for the function type
instance Monad ((-> r)) where
-- return will be the minimal context where the monadic value is still valid.
-- So in this case is just a lambda function that returns the value.
  return x = \_ -> x
-- >>= normally isolates the result from the monadic value, and for this case is
-- not different. To separate the result from the monadic value of a function, we
-- apply the function to the 'w'. So we isolate the value of 'h' by applying 'h w'.
-- That will result in a value. That value is applied to 'f' so we can get the the result
-- of 'f (h w)', which results also in a function. Thats why we apply 'w' again to that.
  h >>= f = \w -> f (h w) w

import Control.Monad.Instances

-- Both functions are the equivalent to 'let f = (+) <$> (*2) <*> (+10)'

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

--  The function above could be rewritten like this, making more explicit how the
--  variable interact with the functions
addStuff :: Int -> Int
addStuff x = let
    a = (*2) x
    b = (+10) x
    in a+b

-- addStuff 3
-- 19
