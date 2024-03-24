import Data.List
import Control.Monad

readMaybe :: (Read a) => String -> Maybe a
-- The function 'reads' returns a list with a single element if successful and an empty
-- list if failure i.e. reads "123" :: [(Int,String)] will return [(123,"")]
-- Uses 'case' to patern match the result of 'reads st'.
-- If the result of 'reads st' was successful and it returned a single tuple with
-- the parsed string and the rest empty, the 'readMaybe' will return the result of 
-- 'reads st' inside a Just.
readMaybe st = case reads st of [(x,"")] -> Just x
-- If the first case does not match, it means 'reads' could not parse the whole 
-- String, so the function will return Nothing.
                                _ -> Nothing


foldingFunction :: [Double] -> String -> Maybe [Double]
-- Pattern match the list of Double into the first element 'x', the second 
-- element 'y', and the rest 'ys'. It also checks if the String is a given symbol.
-- If the symbol matches, use the 'return' function, that sets the default context
-- for the list. 'return' knows what is the default context because of the function
-- type signature that says it has to return a Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x*y):ys)
foldingFunction (x:y:ys) "+" = return ((x+y):ys)
foldingFunction (x:y:ys) "-" = return ((x-y):ys)
-- If the Strig is not any of the symbols it tried to match, it means its a number.
-- 'liftM' will take a function, that in this case is a partially applied list addition,
-- and it also takes a monadic value, that will be the result of 'readMaybe', so it
-- will either be 'Just' or 'Nothing'.
-- 'liftM' will map the function over the Just value, so it will add the number 'readMaybe'
-- return to the list, and keep the list on the same context, which is Maybe
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

-- Takes a String in the Reverse Polish Notation like "1 2 * 4 +" and return a Maybe Double
solveRPN :: String -> Maybe Double
-- Initiate a 'do' notation
solveRPN st = do
-- Take the 'st' and break it into separate elements in a list with 'words' function,
-- and return ["1","2","*","4","+"]
-- 'foldM' is the same as 'foldl', but in the end it returns monadic value. It will apply 
-- the function 'foldingFunction' to each element of the 'words st' result, and 
-- accumulate the return in the end
-- This is also pattern matching saying that the result of the 'foldM' must be a list
-- with a single value
  [result] <- foldM foldingFunction [] (words st)
-- Uses 'return' to the result of the fold, so it can maintain the same context of Maybe Double
  return result

-- solveRPN "1 2 * 4 +"
-- Just 6.0
