-- Pretty much the same as 'fmap', but for Monads. So its for mapping a function over a 
-- monadic value.
liftM :: (Monad m) => (a -> b) -> m a -> m b
-- The monadic value 'm' is fed to the lambda function and then the result of that
-- is applied to 'f'. the result of that is put into the minimal context by 'return'.
liftM f m = m >>= (\x -> return (f x))

-- Same thing but uses the 'do' notation
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = do
    x <- m
    return (f x)

-- liftM (*3) (Just 8)
-- Just 24

-- '<*>' is like 'fmap', but the function is wrapped in a applicative
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
-- Just (+3) <*> Just 4
-- -- Just 7

-- Kind of like 'fmap', but the function also is wrapped in a monadic context.
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
    f <- mf
    x <- m
    return (f x)
-- Just (+3) `ap` Just 4
-- Just 7

