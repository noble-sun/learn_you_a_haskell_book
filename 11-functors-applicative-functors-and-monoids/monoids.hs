
-- Definition of a Monoid
class Monoid m where
-- 'mempty' represents the identity value for a given monoid. Like '1' for '*'
  mempty :: m
-- 'mappend' is a binary function that takes two parameters of the same type and
-- return a third that also have the same type
  mappend :: m -> m -> m
-- 'mconcat' receives a list of monoids and will return one single value.
  mconcat :: [m] -> m
-- It returns one value by folding the list with 'mappend'
  mconcat = foldr mappend mempty

-- Implementation of a monoid for lists
instance Monoid [a] where
-- What is the the identity value for a list
  mempty = []
-- What binaty function with the identity form a monoid
  mappend = (++)
-- 'mconcat' has a default implementation, and for most cases, that implementation
-- is enough

-- This is creating a wrapper for a type 
newtype Product a = Product { getProduct :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

-- Implementation of a monoid for '*' function
-- It has a constraint 'Num a' saying that the type 'Product' is wrapping has to
-- be a number
instance Num a => Monoid (Product a) where
-- The identity is '1', only this is wrapped in the newtype
  mempty = Product 1
-- This is a pattern match that it is taking the values of each 'Product' constructor
-- multiply them and wraps the result in the 'Product' again
  Product x `mappend` Product y = Product (x*y)

instance Monoid Any where
        mempty = Any False
        Any x `mappend` Any y = Any (x || y)

instance Monoid All where
        mempty = All True
        All x `mappend` All y = All (x && y)


instance Monoid Ordering where
-- The identity for the Ordering is 'EQ'
    mempty = EQ
-- Using as example when comparing words 'aba' and 'aab', the first letter is 'EQ', 
-- so we go compare the letters on the right, and in this case the second letter for
-- the left 'b' is greater, so we know that the first word is 'bigger'. Thats why
-- we return 'GT' and don't need to compare the rest of the letters
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT

-- This function compares the length between two strings, and if they are equal,
-- compare the strings by each char.
lenghtCompare :: String -> String -> Ordering
lenghtCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in if a == EQ then b else a

-- Understanding of how the Ordering is a monoid, we can implement the same function
-- in a simpler way.
lengthCompare' :: String -> String -> Ordering
-- If length of 'x' is greater will return 'GT', and if it is less, it will return LT
-- If length of 'x' is equal, will return the comparison of vowels. This is because the
-- monoid implementation for Ordering says that if the left value is 'EQ', it will return 
-- the right value.
-- If vowels of 'x' is 'GT' or 'LT', it will return that vowel comparison.
-- If vowels for 'x' is 'EQ', will return the alphabetical comparison of the words.
-- So it will only compare letter by letter of each word if they have the same quantity
-- of letters and the same quantity of vowels.
lengthCompare' x y = (length x `compare`  length y) `mappend` (vowels x `compare` vowels y) `mappend` (x `compare` y)
  where vowels = length . filter (`elem` "aeiou")

-- This is a type that has two constructors and a field
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

-- Implementation of Foldable to the Tree type
instance F.Foldable Tree where
-- If we try to pass a Empty Tree to the function, the monoid value will be just 'mempty'.
    foldMap f Empty = mempty
-- We recursivelly apply foldMap to the sub-trees with the same function 'f'.
-- We also apply the function 'f' to the Node value of the tree.
-- Since we know that function 'f' has to return a monoid, given the foldMap signature
-- type, we can 'mappend' all the values for the two sub-trees and the node to in the
-- end return one single value
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

-- testTree = Node 5
--             (Node 3
--                 (Node 1 Empty Empty)
--                 (Node 6 Empty Empty)
--             )
--             (Node 9
--                 (Node 8 Empty Empty)
--                 (Node 10 Empty Empty)
--             )

-- F.foldMap (\x -> [x]) testTree
-- [1,3,6,5,8,9,10]
