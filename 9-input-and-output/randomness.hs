import System.Random
import Control.Monad(when)

-- a function that takes a random generator and will return a truple of booleans
threeCoins :: StdGen -> (Bool, Bool, Bool)
-- assign the random generator to 'gen'
threeCoins gen =
-- use the function random with 'gen' and pattern match the return of the function
-- into two 'firstCoin' to the random number generated and 'newGen' to the
-- random generator that the function also returns
  let (firstCoin, newGen) = random gen
-- same as the line above but uses the random generator returned on the first 
-- random function call
      (secondCoin, newGen') = random newGen
-- you get the idea
      (thirdCoin, newGen'') = random newGen'
-- when it calls for firstCoin, secondCoin and thirdCoin the lines above will
-- actually be executed
  in (firstCoin, secondCoin, thirdCoin)
-- we don't need to do something like 'random gen :: (Bool, StdGen)' because
-- since our function type definition already says it has to return Bool values
-- Haskell can use its type inference and do that for us

-- this is a implementation of the function randoms, that generate an infinite amount of random numbers
-- it takes a random generator and returns a list of random numbers
randoms' :: (RandomGen g, Random a) => g -> [a]
-- assign the random generator it receives to 'gen'
-- when we call the function, everything in the 'in' block is executed
-- so that means it wants the value for the 'value', that is the head of a list
-- since the type definition tells us it has to return a list. I'm kind of assuming this.
-- to get the 'value', the 'random' function is executed
-- so now we have the head value, but we need the contents of 'tail', which is 
-- a recursive call to our function using the new random generator from the first call of 'random'
-- if I understand correctly, since our function does not have any edge condition,
-- if will run infinitly
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

-- since its a function called 'main', the 'do' is mandatory. It glue several
-- IO actions so that we can have more than one action in the same function
main = do
-- this binds something to the 'gen' name
-- 'getStdGen' retrieves a random generator that Haskell stores globaly when the 
-- program is started.
  gen <- getStdGen
  askForNumber gen

-- it takes a random generator and returns an IO action
askForNumber :: StdGen -> IO ()
-- assign the number generator it receives to 'gen' and initiates a 'do' block
-- to allow for multiple IO action
askForNumber gen = do
-- generates a random Int number between 1 to 10 using the 'randomR' function and
-- use pattern match to assign the return of that function to 'randNumber' being the
-- random integer number it generated and 'newGen' the new random generator it also returns
  let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
-- print a string to the standard, this being the terminal
  putStr "Which number in the range from 1 to 10 am I thinking of? "
-- bind the line that the user types in the terminal to 'numberString'
-- this assumes the user will only type the number it is guessing
  numberString <- getLine
-- not entirely sure what this line does, but we can assume that it is checking if
-- the 'numberString' is not empty, and if it is end the program?
  when (not $ null numberString) $ do
-- transform the string number to an actual number e.g. '1' to 1
    let number = read numberString
-- simple conditional to check if the random number that was generated is the same
-- as the one the user guessed
    if randNumber == number
-- If it is, print in a new line a string
        then putStrLn "You are correct!"
-- If it is not, print in a new line a string with the random number
        else putStrLn $ "Sorry, it was " ++ show randNumber
-- call recursivelly the function with the new random generator from the first call
-- of the function 'randomR'
    askForNumber newGen



