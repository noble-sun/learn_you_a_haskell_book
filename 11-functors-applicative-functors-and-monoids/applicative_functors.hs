import Control.Applicative

-- definition of the Applicative typeclass
-- Has a constraint that says that to be part of the Applicative typeclass, it 
-- has to be a Functor too
class (Functor f ) => Applicative' f where
-- pure will take a parameter of any type and wrap this parameter in a applicative functor
  pure :: a -> f a
-- <*> is just like fmap, but instead of taking a function, it takes a functor that
-- has a function inside. It will then 'extract' the function that's inside the functor
-- to map it over the second functor.
  (<*>) :: f (a -> b) -> f a -> f b

-- Applicative instance implementation for Maybe
instance Applicative Maybe where
-- As the definition above says, 'pure' wraps something in a applicative functor. Since 'Just'
-- is a normal function we can implement as it is, but we could have also written as 'pure x = Just x'
  pure = Just
-- Since it wouldn't have a function on a 'Nothing', we can say that it would simply return 'Nothing'
  Nothing <*> _ = Nothing
-- The definition of '<*>' says that both parameters have to be a functor, so if the first 
-- parameter is anything other than Nothing, we want to map the function that's inside
-- the first parameter over the sencond parameter
  (Just f) <*> something = fmap f something


-- pure (+) <*> Just 3 <*> Just 5
-- 'pure (+)' has a type 'a :: (Applicative f, Num a) => f (a -> a -> a)', so
-- it says that it will take two parameters that are of Num type, an return something that
-- is also a Num, and wrap that function over a applicative functor. It knows it
-- has to be a number because of the function type definition. At this step, we
-- only know it will wrap that function over a functor, but we don't know which one.
-- <*> is left-associative, so 'pure (+) <*> Just 3' is evaluated first, and then
-- that result is evaluated with 'Just 5'.
-- If we look a 'pure (+) <*> Just 3' type definition 'a :: Num a => Maybe (a -> a)',
-- we see that now Haskell knows pure will wrap the Maybe functor, because it can
-- infer thanks to the 'Just 3'. In the end this will result in 'Just (3+)'.
-- So now 'Just (3+) <*> Just 5' is evaluated. If we look at the definition of 
-- '<*>' for the Maybe type, it will take '(3+)' and fmap over the second parameter,
-- which is 'Just 5'. fmap will unwrap 'Just 5' to be simply '5', run the function
-- (3+) over it, and since its a fmap, it has to return a functor, so it wraps 
-- 'Just' over the result, which is 'Just 8'

-- definition of '<$>' which is the infix form of fmap
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
-- its saying that it will apply the first parameter over the second using 'fmap'
f <$> x = fmap f x

-- (++) <$> Just "johntra" <*> Just "volta"
-- '(++) <$> Just "johntra"' come first, it will have a type of '(++) <$> Just "johntra" <*> Just "volta"',
-- and will result on 'Just ("johntra++)'.
-- 'Just ("johntra++) <*> Just "volta"', is then evaluated and results on 'Just "johntravolta'

-- Applicative instance implementation for []
instance Applicative [] where
-- just makes the value a singleton list
  pure x = [x]
-- This assumes that 'fs' is a list of functions.
-- Its a list comprehension where it will that each value of 'fs' for each value of 
-- 'xs', and will return the applied result of the function 'f' for each value of 'x'
  fs <*> xs = [f x | f <- fs, x <- xs]


-- Applicative instance implementation for '(->) r', or function
instance Applicative ((->) r) where
-- 'pure' is the minimal default context that is a value, so for the pure implementation
-- for functions, its just a function that ignores the parameters and return the value
    pure x = (\_ -> x)
-- 'f' is a function, and 'g' also is a function. Given 'x' it will apply the function 'f'
-- to 'x', and then apply the function 'g' to 'x'. It will then apply those results to each other.
    f <*> g = \x -> f x (g x)

-- :t (+) <$> (+3) <*> (*100)
-- (+) <$> (+3) <*> (*100) :: (Num a) => a -> a
-- (+) <$> (+3) <*> (*100) $ 5
-- First '(+) <$> (+3)', this would give a function that its waiting for an argument to add 
-- to the left parameter, and then apply '+' to the result.
-- Second '<*> (*100)', a function that is waiting for an argument to be multiplied by 100.
-- So up to this point it would be something like '(\x -> (+) (x + 3) (100 * x))'. So
-- what it will do is sum the result of both functions when they are evaluated.
-- Third '$ 5', take 5 as an argument for the functions an evaluated all the expression

-- Applicative instance implementation for ZipList
instance Applicative ZipList where
-- results in an infinite list of the argument
  pure x = ZipList (repeat x)
-- it takes two lists and apply the function on the first list to the element of 
-- the second list, for the current position of the element. So apply the first function to
-- the first element, the second function to the second element, ect, resulting in a list
-- the same size as the shorter of the two lists
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)



liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
-- liftA2 (:) (Just 3) (Just 4) results in 'Just [3,4]'

-- Will transform a list of applicatives into a applicative with a list
sequenceA :: (Applicative f) => [f a] -> f [a]
-- Edge condition for the recursion, if the list is empty, return an empty list 
-- with the default context
sequenceA [] = pure []
-- Call the function again with the tail of the list of applicatives.
-- Eventually will reach the edge condition and return the empty list, so 'Just []'.
-- With the empty list, it will use the head end prepend it to the list.
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- Let's use the example 'sequenceA[Just 1, Just 2]'
-- (:) <$> Just 1 <*> sequenceA [Just 2]
-- (:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])
-- (:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])
-- (:) <$> Just 1 <*> Just [2]
-- Just [1,2]

-- It could also be implemented with a fold
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

-- sequenceA can be used with functions too, since a function is also a functor.
-- sequenceA [(+3),(+2),(+1)]. This will create a function that takes one parameter
-- and apply that parameter for each functor, or in this case, for each partially
-- applied function.
-- So if we call with 'sequenceA [(+3),(+2),(+1)] 3', it would resut in [6,5,4]

-- How about a list of lists like 'sequenceA [[1,2],[3,4]]'
-- (:) <$> [1,2] <*> sequenceA [[3,4]]
-- (:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])
-- (:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])
--                    (:) <$> [3,4] <*> [[]]
--                    [3:[], 4:[]]
--                    [[3],[4]]
-- (:) <$> [1,2] <*> [[3],[4]]
-- [1:[3], 1:[4], 2:[3], 2:[4]]
-- [[1,3],[1,4],[2,3],[2,4]

-- sequenceA for a list of IO actions, works the same a the sequence function
-- sequenceA [getLine, getLine, getLine]
-- hey
-- ho
-- woo
-- ["heyh","ho","woo"]
