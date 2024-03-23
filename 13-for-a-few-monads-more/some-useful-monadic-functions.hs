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

-- flatten one monadic value that's inside another monadic value
-- The function type says that the both monads have to be of the same type
join :: (Monad m) => m (m a) -> m a
join mm = do
-- This line takes care of the context of the monad and extract the inned monadic value.
-- This is the same as 'mm >>= \m -> ...'
    m <- mm
-- Return just the inner monadic value that was binded above.
    m

-- m >>= f is the same as join (fmap f m)
-- runWriter $ join (Writer (Writer (1,"aaa"),"bbb"))
-- (1,"bbbaaa") 

-- join (Right (Right 9)) :: Either String Int
-- Right 9 

-- runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0]
-- ((),[10,1,2,0,0,0]) 

-- Takes a function that accepts a value and return a monadic boolean type, a list of
-- values and return a list of values with a monadic context.
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

-- Takes a number and return a Writer monad that has a String as a monoid and a Bool
-- as the value
keepSmall :: Int -> Writer [String] Bool
-- Name the Int value as 'x'
keepSmall x
-- use guards to check for the value and initiate a 'do' notation for each guard.
  | x < 4 = do
-- Inside a 'do' notation every line must be a monadic value.
-- 'tell' is just the same as a dummy '()' monatic value, and we log the string to it.
      tell ["Keeping " ++ show x]
-- 'return' has to be the minimal default context for the thing, since the type the function
-- return is a Writer, return has to be a True value that has the Writer context.
      return True
  | otherwise = do
      tell [show x ++ " is too large, throwing it away"]
      return False

-- take the first element of the tuple that is the result of 'runWriter'. 'runWriter' is
-- a function from the Writer monad, that extract the values from the Writer to be
-- shown as a tuple
-- fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
-- [1,2,3]

-- mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
-- 9 is too large, throwing it away
-- keeping 1

-- This function wil drop and keep every element, so it will for example, both keep 
-- and drop '1' from the list, both keep and drop '2'. So in the end the list will
-- have all the possible combinations of elements.
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs
-- powerset [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

-- Takes two Int values, where one is the accumulator and one the value we want 
-- to check. I returns a Maybe Int
binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
-- Use guards to check the value with a predicate. If its true, return nothing
  | x > 9     = Nothing
-- If the value its bigger than 9, sum the two values together in a Just.
  | otherwise = Just (acc + x)

-- foldM binSmalls 0 [2,8,3,2]
-- Just 14
-- foldM binSmalls 0 [2,11,3,1]
-- Nothing

