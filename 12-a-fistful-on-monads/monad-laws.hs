-- First Monad Law
-- Left Identity
-- 'return x >>= f' is the same as 'f x'
-- The two expressions must be equivalent for a type to be a Monad
-- 'return' puts a value in its minimal context, and or Maybe that's 'Just'. '>>='
-- will remove the value from the 'Just' to apply to the lambda function.
return 3 >>= (\x -> Just (x+10000))
(\x -> Just (x+10000)) 3

-- Second Monad Law
-- Right Identity
-- 'm >>= return' is the same as 'm'
-- The result of the expression must be the same as the list being passed on '>>='.
-- With how '>>=' is implemented for lists, it will apply 'return' for every element 
-- of the list, resulting in [[1],[2],[3],[4]], and after it will apply 'concat' to
-- this result, returning [1,2,3,4] in the end.
[1,2,3,4] >>= (\x -> return x)

-- Third Monad Law
-- Associativity
-- (m >>= f) >>= g is the same as m >>= (\x -> f x >>= g)
-- When composing two funtions, the implementation is like this:
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = (\x -> f (g x))
-- So 'g' type is '(a -> b)', and 'f' type is '(b -> c)'. When composing those two 
-- functions, we rearrenge so tha the result of 'g' which is 'b', is applied as 
-- a parameter of 'f', which is also 'b', so that the type of the composed function 
-- is '(a -> c)'.
-- If 'f' and 'g' returned a monadic value, we wouldn't be able to compose these functions,
-- because 'g' would return 'm b', and 'f' would take just 'b' and not 'm b'.
-- We can achieve that by using '<=<'
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x g x >>= f)

-- let f x = [x, -x]
-- let g x = [x*3, x*2]
-- let h = f <=< g
-- h 3
-- [9, -9,6,-6]
