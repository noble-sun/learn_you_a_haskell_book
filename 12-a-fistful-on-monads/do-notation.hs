foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

-- We can use the 'do' notation to rewrite the function above, where every line
-- in the 'do' expression is a monadic value.
-- The last line of the do expression is the result of the whole chained monadic 
-- expression.
fooDo :: Maybe String
fooDo = do
      x <- Just 3
      y <- Just "!"
      Just (show x ++ y)

-- Just 9 >>= (\x -> Just (x > 8))
-- The expression above is another example showing how the last line of the 'do'
-- expression is the result of the whoe monadic expression.
marySue :: Maybe Bool
marySue = do
  x <- Just 9
  Just (x > 8)

routine :: Maybe Pole
routine = do
  start <- return (0,0)
  first <- landLeft 2 start
-- If we wanted to  throw an 'failure', we can just write 'Nothing'
-- When we write a line and don't bind the monadic value with '<-', it would be the 
-- equivalent to putting '>>' after the monadic value whoe result we want to ignore.

-- So the line below would be akin to '_ <- Nothing'
  Nothing
  second <- landRight 2 first
  landLeft 1 second

-- This would be the equivalent of the function above, but without the monadic 
-- aspects of 'Maybe'.
routineDoless :: Maybe Pole
routineDoless =
  case Just (0,0) of
    Nothing -> Nothing
    Just start -> case landLeft 2 start of 
      Nothing -> Nothing
      Just first -> case landRight 2 first of
        Nothing -> Nothing
        Just second -> landLeft 1 second

-- It is possible to use pattern matching inside the 'do' notation.
justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x

-- This function will always result in a pattern match failure. When inside a
-- 'do' notation and a pattern match fails, the function 'fail' is called for 
-- the given Monad, in this case 'Maybe'.
-- The implementation for the 'fail' function for 'Maybe' just returns 'Nothing'.
wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x

