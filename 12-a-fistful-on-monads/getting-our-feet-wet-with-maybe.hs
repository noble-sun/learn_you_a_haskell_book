-- This is just a function that doing what '>>=' would do for the Maybe type
-- (>>=) :: (m a) => m a -> (a -> m b) -> m b
-- It follows the same type definition, but it already says that the Monad is the 
-- Maybe type
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Just like for the applicative functor for Maybe, if it's Nothing, there wouldn't
-- be a value to apply the function, so it returns Nothing
applyMaybe Nothing f = Nothing
-- Pattern match the Maybe to extract the value that it's wrapped in the Maybe and
-- apply the function to that value
applyMaybe (Just x) f = f x
-- If the function itself return Nothing, the result of 'applyMaybe' would also
-- be Nothing, even if the first parameter 'Maybe a' is a Just value

-- Just 3 `applyMaybe` \x -> Just (x+1)
-- Just 4
