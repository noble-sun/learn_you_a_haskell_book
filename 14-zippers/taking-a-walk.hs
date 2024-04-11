-- a new type named 'Tree', where it can be empty or be a node. If its a node
-- it has a value and two other Trees. 
-- The type derives from Show, so it can be shown in the standard output
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

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

-- This function pattern match the Tree to change the value of one of the Nodes.
changeToP :: Tree Char -> Tree Char
--              P node_L  L       W C R  O
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToPd :: Directions -> Tree Char -> Tree Char
changeToPd (L:ds) (Node x l r) = Node x (changeTop ds l) r
changeTopd (R:ds) (Node x l r) = Node x l (changeTop ds r)
changeToPd [] (Node _ l r) = Node 'P' l r
