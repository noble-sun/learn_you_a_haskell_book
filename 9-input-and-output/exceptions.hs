import System.Environment
import System.IO
import System.Directory
import System.IO.Error
import Control.Exception

mainOld = do
  (fileName:_) <- getArgs
  fileExists <- doesFileExist fileName
  if fileExists
      then do contents <- readFile fileName
              putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
      else do putStrLn "The file doesn't exist!"


-- using the 'catch' function to treat IO errors 
-- if some error is thrown in the function 'toTry', the IOError is passed to 'handler' treat
main = toTry `catch` handler

-- the function simply execute IO actions, so its type is just IO ()
toTry :: IO ()
-- wraps all IO action into one with 'do' to allow more then one action
toTry = do
-- pattern match the list that 'getArgs' return to get only the first argument 
-- when the program is executed, and give the name 'fileName'. Ignore the rest of the 
-- arguments if more are passed
  (fileName:_) <- getArgs
-- binds the contents of the file
  contents <- readFile fileName
-- print in the standard location (terminal)
-- use function application '$' so that all the text is concateneted before it prints
-- 'lines contents' splits the content into a list, and then count the number of elements,
-- and "converts" the number to be able to be printed
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

-- It takes an IOError and return an IO action
handler :: IOError -> IO ()
-- attributes the IOError to 'e'
handler e
-- use guards to pattern match errors
-- checks if 'e' is of this 'IOError' "type"
  | isDoesNotExistError e =
-- evaluates if it could retrieve the file name
-- if the file path if "found" print something, and if it return Nothing, print
-- another thing
      case ioeGetFileName e of Just path -> putStrLn $ "Whooops! File does not exist at: " ++ path
                               Nothing -> putStrLn "Whooops! File does not exist at unknown location!"
-- catch all pattern match guard
-- 'ioError' will re-throw the error
  | otherwise = ioError e
