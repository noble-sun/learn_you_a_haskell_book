--- a new data type for Tree.
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)  
-- A new data type that can be either 'L' or 'R'. Similar to Bool, that can be
-- 'True' or 'False'.
data Direction = L | R deriving (Show)

-- a new data type that can be two different types, 'LeftCrumb' that has two fields,
-- a element and a Tree. 'RightCrumb' follows the same structure
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

-- Another alias to represent a list of Crumb
type BreadCrumbs a = [Crumb a]

-- The structure of having a focused section of a data and its surroundings is
-- commonly called Zipper. So we created an alias for that.
type Zipper a = (Tree a, BreadCrumbs a)

-- These functions are pretty similar to the other ones, the only difference is 
-- that it return a Zipper wrapped in a monad Maybe
goLeft :: Zipper a -> Maybe (Zipper a)
-- It logic behind is the same, it just wraps the result in a 'Just'
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
-- If the tree is Empty, it just ignores everything else and return 'Nothing'
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
-- If the list is empty, it means that there's no breadcrumbs left, which also means 
-- that we are at the root of the tree, which also means that we can't go up in the 
-- tree. So we return 'Nothing'
goUp (_, []) = Nothing

-- Since they are now wrapped in a Monad, we use '>>=' instead of '-:'
-- let coolTree = Node 1 Empty (Node 3 Empty Empty)  
-- return (coolTree,[]) >>= goRight  
-- Just (Node 3 Empty Empty,[RightCrumb 1 Empty])  

-- return (coolTree,[]) >>= goRight >>= goRight  
-- Just (Empty,[RightCrumb 3 Empty,RightCrumb 1 Empty])  

-- return (coolTree,[]) >>= goRight >>= goRight >>= goRight  
-- Nothing  
