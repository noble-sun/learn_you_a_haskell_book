import Data.List

solveRPN :: String -> Float
-- this is how we could write the function without using the point-free style
-- solveRPN expression = head (foldl foldingFunction [] (words expression))

-- we're using function composition to remove some parenthesis, and this makes 
-- the expression to be right oriented (I think?).
-- since we're using the point-free style to write this function, we can omit the
-- argument naming, as Haskell can assuume that it will be at the end of the expression.
-- so 'words' will split the argument we receive when calling the function 'solveRPN'
-- into a list separated by spaces.
-- 'foldl' is a function that takes three parameters, but we're only calling it with two, kind of.
-- the third argument is the result of the 'words' function, that we can use as the missing
-- parameter because of the '.', which is the symbol for the function composition.
-- in the same fashion as before, 'head' only take one parameter, but because of the 
-- function composition, it has to wait for the result of the 'foldl' function.
solveRPN = head . foldl foldingFunction [] . words
-- pattern match using where to execute the correct operation
-- 'foldl' will take a function, which in this case is is defined in a where bind.
-- that function will then take the accumulator list, and the first element starting from
-- the left of the result of the 'words' function. It will take the first element 
-- because 'foldingFunction' itself is being executed as part of the 'foldl' function,
-- that will run this function for every element of a list.
  where foldingFunction (x:y:ys) "*" = (x * y):ys
-- It will separate the accumulator list into the first element 'x', the second 
-- element 'y' and the rest of the list 'ys'
-- If the next element of the list being folded is "+", sum x + y and place as head of 
-- the accumulator list
        foldingFunction (x:y:ys) "+" = (x + y):ys
        foldingFunction (x:y:ys) "-" = (y - x):ys
        foldingFunction (x:y:ys) "/" = (y / x):ys
        foldingFunction (x:y:ys) "^" = (y ** x):ys
        foldingFunction (x:xs) "ln" = log x:xs
        foldingFunction xs "sum" = [sum xs]
-- if none of the matchings are met, it means the element is not an operator, so
-- we 'read' it to transform into a number (that will have a type Float because
-- of the type definition of our function) and place it as head of the accumulator
        foldingFunction xs numberString = read numberString:xs


