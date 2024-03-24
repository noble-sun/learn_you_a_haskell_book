import Control.Monad
-- An alias for a tuple of Int
type KnightPos = (Int, Int)

-- Function that will take the initial position of a knight and return all the
-- possible positions to where it can move to.
moveKnight :: KnightPos -> [KnightPos]
-- use pattern match to name the values of the tuple it receives and starts a
-- 'do' notation, since we are working with lists, and lists are Monads.
moveKnight (c,r) = do
-- "c'" and "r'" represent every possible position of the list its being assigned to.
  (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
               (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
-- 'guard' is checking if for each "c'" and "r'" of the positions list are within the
-- dimentions of the chess board. If its not, it results in a empty list and that 
-- position is not shown in the result.
  guard (c' `elem` [1..8] && r' `elem` [1..8])
-- return the positions available
  return (c',r')

-- This can be also be done with filter
moveKnightFilter :: KnightPos -> [KnightPos]
moveKnightFilter (c,r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
    ]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

-- This function just run the function 'moveKnight' three times to see all the
-- possible positions the knight can be after three moves
in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

-- Without using 'do' notation
in3Doless start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- Take the initial knights position and the end position you want to check if it
-- can reach in three moves, and return True of False whether it can or not.
canReachIn3 :: KnightPos -> KnightPos -> Bool
-- pattern match to name the parameters.
-- evaluates the function 'in3' taking the start position, and checks if the 'end'
-- position is in the result of the list of possible positions after three moves.
canReachIn3 start end = end `elem` in3 start




-- The first parameter it takes is how many moves the knight will make, the second parameter
-- is the initial position of the knight, and the result is a list of all the
-- possible end positions after that number of moves.
inMany :: Int -> KnightPos -> [KnightPos]
-- 'replicate' the function 'moveKnight' 'x' number of times.
-- '<=<' is the operator for composing monadic functions. So 'foldr' will do something like
-- '[moveKnight, moveKnight, moveKnight]' and make it 'moveKnight <=< moveKnight <=< moveKnight'.
-- That composition will then be applied to the 'start' position, to get all the possible
-- positions after that many moves.
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

-- The same as 'canReachIn3', only it takes how many moves you want to make the 
-- knight take.
canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

