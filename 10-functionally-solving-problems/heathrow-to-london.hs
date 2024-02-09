-- import to have access to a whole bunch of functions for manipulating lists
import Data.List

-- Creates a new type Section and it only has one constructor with the same name
-- that has three Int fields, so we would do something like Section 10 4 3. The type 
-- Section derives from the typeclass Show, so it can be represented as a String
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
-- This creates a alias or a synonym type. Just like a String is the same as [Char],
-- RoadSystem is a list of sections like [Section 2 15 5, Section 1 9 44, Section 10 32 18]
type RoadSystem = [Section]
-- We're creating another type, but this time it has three contructors and its similar 
-- to the Bool type. Bool can be either True or False, Label would work the same, but 
-- it can be A or B or C. It also derives from Show, so it can have a string representation
data Label = A | B | C deriving (Show)
-- Another type synonym, an it will be a list of tuples that have a Label and an Int, so
-- something like [(B, 19), (A, 4), (A, 17), (C, 10)]
type Path = [(Label, Int)]

-- takes a tuple of Paths and a Section, and return a tuple of Paths
roadStep :: (Path, Path) -> Section -> (Path, Path)
-- pattern match the parameters and name them
roadStep (pathA, pathB) (Section a b c) =
-- use let bindings to make things more readable
-- 'priceA' will sum the result map returns, that's because of the function application '$'
-- that makes the function right associative, so it has to evaluate everything that is on 
-- the right side of the '$'
-- The map itself will take the second element of every pathA. Just remembering that pathA 
-- is a Path, and Path is a list of tuples [(A, 3), (C, 8), (B, 10)]. 'snd' is a tuple function
-- that will return the second element of the tuple. So map will return a list of Ints
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
-- This is adding the values of the paths to the next point of the main road A and B.
-- To go to Ax we can directly take the main road a
      forwardPriceToA = priceA + a
-- Or to go to Ax we can go from de main road B, go forward and then cross to reach Ax 
      crossPriceToA = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
-- check which one is the shortest path to reach the next point on the road A,
-- going straight forward or going from road B and cross it to A.
      newPathToA = if forwardPriceToA <= crossPriceToA
-- if going straight forward is the shortest add a new tuple to pathA, which is 
-- basicaly saying that the next move is to take the main road A, and the value of
-- the action. Since pathA is a list, we can use 'cons', also known as ':' to add 
-- to the beginning of the list
                      then (A, a):pathA
-- if crossing is the shortest, add two new tuples, but this time add it to pathB.
-- It says, go forward from B and them take the cross to reach road A
                      else (C, c):(B, b):pathB
      newPathToB = if forwardPriceToB <= crossPriceToB
                      then (A, a):pathB
                      else (C, c):(A, a):pathA
-- This will trigger the evaluation of all the bindings above to get the values of 
-- newPathToA and newPathToB, and return as a tuple of Paths.
-- So this will actually return ([(),()],[(),()])
  in (newPathToA, newPathToB)

-- This function will receive a RoadSystem, which is a list of Sections and return
-- the best path
optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
-- 'foldl' will run the roadStep function for each roadSystem, that is, a Section, and 
-- the accumulator would be the best path of each road
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
-- sum the second value of each tuple of 'bestAPath' and 'bestBPath' and check which one
-- is the shortest
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
  -- if 'bestAPath' is the shortest, we return the reversed list. That's because
  -- 'roadStep' add the next best action to the beginning of the list, because it is
  -- much more efficient than adding to the last position of a list.
          then reverse bestAPath
          else reverse bestBPath

-- It takes an Int, a list of something, and will return a list of lists
groupsOf :: Int -> [a] -> [[a]]
-- pattern match if the first parameter is '0', if it is, the second parameter wouldn't
-- even matter to us, since we need the first one to be a positive integer
groupsOf 0 _ = undefined
-- return an empty list if the second parameter is also an empty list
groupsOf _ [] = []
-- will take the first 'n' elements of the list, so this would be [a,b,c]. Add this to
-- the beginning of a list with ':'. So for the tail of that list it just added something,
-- it will call again the function 'groupsOf' passing the 'n' and a list without those
-- 3 elements we just 'took'.
groupsOf n xs = take n xs : groupsOf n (drop n xs)

-- 'do' block to allow multiple IO actions
main = do
-- bind all the contents that are in the standard input to 'contents'
  contents <- getContents
-- convert 'contents' to a list where each element is a line, and pass a 'map' to 
-- transform all the elements of the list into Ints. Call 'groupsOf' to separate
-- that list into a list of list of three elements each
  let threes = groupsOf 3 (map read $ lines contents)
-- Will take the list of lists of three elements each and for each list will execute the 
-- lambda function, that is simply passing each element of the list and creating a Section
-- passing the arguments
      roadSystem = map (\[a,b,c] -> Section a b c) threes
-- Find the best path 
      path = optimalPath roadSystem
-- 'map' will return all the first value of each tuple, and run 'show' to have the string 
-- representation of the type, that's because the first value of the tuple is a Label type. Then
-- will merge all the elements of the map together.
      pathString = concat $ map ( show . fst) path
-- take sum all the second values of the tuples
      pathPrice = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice
