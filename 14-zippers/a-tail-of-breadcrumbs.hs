-- a new data type for Tree.
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)  
-- A new data type that can be either 'L' or 'R'. Similar to Bool, that can be
-- 'True' or 'False'.
data Direction = L | R deriving (Show)
-- A alias for a list of 'Direction' types.
type Breadcrumbs = [Direction]

-- This is just a function that gives a structured tree example
freeTree :: Tree Char  
freeTree = 
  Node 'P'  
    (Node 'O'  
      (Node 'L'  
        (Node 'N' Empty Empty)  
        (Node 'T' Empty Empty)  
      )  
      (Node 'Y'  
        (Node 'S' Empty Empty)  
        (Node 'A' Empty Empty)  
      )  
    )  
    (Node 'L'  
      (Node 'W'  
        (Node 'C' Empty Empty)  
        (Node 'R' Empty Empty)  
      )  
    (Node 'A'  
      (Node 'A' Empty Empty)  
      (Node 'C' Empty Empty)  
    )  
  )  

-- This function takes a tuple, where the first element is a Tree and the second is
-- a Breadcrumb (list of directions). In the end it will also return a tuple with
-- a Tree and a Breadcrumb.
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
-- pattern match the Tree and ignore the root element and the right node. 
-- Name the Breadcrumb as 'bs'.
-- Return the entire left Tree as the first element of the tuple and add a 'L' to
-- the head of the list in the second element of the tuple.
goLeft (Node _ l _, bs) = (l, L:bs)

-- Same as the 'goLeft' function but applied to the right side Tree.
goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

-- Makes the syntax a little better to understand the steps.
-- So instead of:
-- goLeft (goRight (freeTree, []))
-- we can do (freeTree, []) -: goRight -: goLeft.
-- Both of which will result in
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])
x -: f = f x

-- a new data type that can be two different types, 'LeftCrumb' that has two fields,
-- a element and a Tree. 'RightCrumb' follows the same structure
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
-- Another alias to represent a list of Crumb
type BreadCrumbs a = [Crumb a]

-- A slight variation of the 'goLeft' function. It uses the new data type to 
-- basically store the other Trees we didn't got to. So instead of just having
-- the direction we went, we have the value of the root element of the node we
-- receive, and the whole right Node we previously ignored.
goLeftCrumb :: (Tree a, BreadCrumbs a) -> (Tree a, BreadCrumbs a)
goLeftCrumb (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRightCrumb :: (Tree a, BreadCrumbs a) -> (Tree a, BreadCrumbs a)
goRightCrumb (Node x l r, bs) = (r, RightCrumb x l:bs)

-- This function takes a Tree and reconstructs the parent node using the BreadCrumbs.
goUp :: (Tree a, BreadCrumbs a) -> (Tree a, BreadCrumbs a)
-- Pattern match the Tree as 't'. Also pattern match to see if the BreadCrumb is a LeftCrumb.
-- Reconstruct the parent node using the LeftCrumb, passing the 't' as the left Node,
-- taking the right node from the BreadCrumb.
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

-- The structure of having a focused section of a data and its surroundings is
-- commonly called Zipper. So we created an alias for that.
type Zipper a = (Tree a, BreadCrumbs a)

-- This function receives a function and a Zipper, and returns a new Zipper
modify :: (a -> a) -> Zipper a -> Zipper a
-- If the Zipper is a Node, apply the function to the root element of the Node.
modify f (Node x l r, bs) = (Node (f x) l r, bs)
-- If the Zipper is Empty, just return the same Zipper it received
modify f (Empty, bs) = (Empty, bs)

-- let newFocus = modify (\_ -> 'P') (goRight (goLeft (freeTree,[])))  
-- let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')  

-- let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')  


-- Takes a Tree and a Zipper, and returns a new Zipper
attach :: Tree a -> Zipper a -> Zipper a
-- Pattern match the Zipper to ignore the Tree, independently if it is a Node or Empty.
-- Replaces that Tree with the one we took as parameter.
attach t (_, bs) = (t, bs)
-- let farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft  
-- let newFocus = farLeft -: attach (Node 'Z' Empty Empty)


-- This function craws all the way to the top of the Tree
topMost :: Zipper a -> Zipper a
-- If the BreadCrumb is an empty list, it means that we already are at the top, so 
-- we just return the same thing.
topMost (t,[]) = (t,[])
-- If the BreadCrumb is not empty, we take the Zipper and name it 'z'. We 
-- recursivelly call 'topMost', and instead of calling it with 'z', we first call
-- the 'goUp' with 'z'. The 'goUp' will basically return the parent node of the 
-- current node Zipper have.
-- So each time we reconstruct the parent when recursivelly calling this function.
topMost z = topMost (goUp z)

