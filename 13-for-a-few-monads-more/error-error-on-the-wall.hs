-- instance implementation of the Monad for Either.
-- It has a constraint where 'e' also has to be of type class 'Error'.
-- The 'Error' type class is for values that can act like error messages.
instance (Error e) => Monad (Either e) where
-- Minimal default context where the value is still valid. Just wraps the result
-- in a `Right`
     return x = Right x 
-- '>>=' has two matches, one for the successful result, which is 'Right', and one
-- for the failure result, which is 'Left'.
-- For 'Right', it will isolate the value from it and then apply it to the function.
-- For 'Left', it will just ignore the function and return the whole 'Left' with the error.
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
   
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Right (left+n, right)
    | otherwise                    = Left ("There are " ++ show (left+n) ++ " birds on the left side, and " ++ show right ++ " birds on the right side")

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
    | abs (left - (right+n)) < 4 = Right (left, right+n)
    | otherwise                  = Left ("There are " ++ show left ++ " birds on the left side, and " ++ show (right+n) ++ " birds on the right side")

