import System.Environment
-- import the bytestring module with an alias
import qualified Data.ByteString.Lazy as B

-- main functions have to use the 'do' block
-- The 'do' block allow for multiple IO action in a function
main = do
-- 'getArgs' get the arguments passed to the program
-- This is splitting a list using pattern match. Binds the first argument to 
-- 'fileName1', the second argument to 'fileName2' and ignore any other argument
-- it passes 
  (fileName1:fileName2:_) <- getArgs
-- call the function 'copyFile'
  copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO()
-- takes the two arguments it receives and assign the names 'source' and 'dest' respectively
copyFile source dest = do
-- binds the content of the file to 'contents'. Its using bytestring module to read the 
-- file, instead of the string one, so it will read it in chunks.
  contents <- B.readFile source
-- write 'contents' also using the bytestring module to the 'dest' file
  B.writeFile dest contents
-- What the program is doing is reading one file in 64k chunks, writing these
-- 64k bits to another file. At this point, this last line will say to Haskell
-- "hey is there any more of this 'contents' for me to write?", and then it will
-- load another 64K bits into memory for it to write
