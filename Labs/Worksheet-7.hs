import System.Environment
import System.IO
import System.IO.Error

-- BASIC IO INTERACTION ---------------------------------------------------------------------------
-- Print output to the command line 
--putStr :: String -> IO ()
--putStrLn :: String -> IO ()

-- 1 print a hello world
helloWorld :: IO()
helloWorld = putStrLn "Hello World!!"

-- a)
sayHello :: String -> String 
sayHello name = "Hello " ++ name ++ "!"
-- example ghci> sayHello "Damian" // Hello Damian!

-- b) says hello to a name entered in the command line
greet :: IO()
greet = do
    name <- getLine
    putStrLn (sayHello name)

-- c)
greet' :: IO()
greet' = do 
    putStr "Whats your name?"
    greet 

-- d) getInt :: IO Int which reads a line from the command line and converts it to an integer
getInt :: IO Int
getInt = do  
        line <- getLine
        return (read line)

-- 2 FILE IO, MAIN AND COMMAND LINE ARGUMENTS ---------------------------------------------------------------------------

-- main :: IO () => The entry point for any Haskell program executed on the command line is
--main :: IO ()

-- obtain command line arguments from the system.enviroment
--getArgs :: IO [String]

-- a) prints all the arguments passed to the program back to the command line
echoArgs :: IO ()
echoArgs = do 
          args <- getArgs
          print args
-- example ghci> echoArgs "Damian" "Kowalski" // [ "Damian", "Kowalski" ]

-- b)  compile and run a program echo.hs which echos all the arguments passed to it.
echoArgs' :: IO ()
echoArgs' = do 
            args <- getArgs
            putStrLn (unwords args) -- unwords places a space between each element in the provided lis
-- example ghci> echoArgs' "Damian" "Kowalski" // Damian Kowalski

-- WRITE AND READ FILES --------------------------------------------------------------------------------

-- a) which preprends "> " on to the front of every string in the provided list
addLineStart :: [String] -> [String]
addLineStart = map ("> " ++)
-- example ghci> addLineStart ["Damian", "Kowalski"] // ["> Damian", "> Kowalski"]

-- b) addLineStartToFile contents first uses lines to break up contents, 
--then adds line starts to all the lines and then joins it back together using unlines
addLineStartToFile :: String -> String 
addLineStartToFile = unlines . addLineStart . lines
-- example ghci> addLineStartToFile "Damian\nKowalski" // "> Damian\n> Kowalski"

--c )  reads the file hs, preprends "> " to every line in the file, 
--and writes it back out to the file lhs.
hsToLhs :: String -> String -> IO ()
hsToLhs hs lhs = do
    contents <- readFile hs
    writeFile lhs (addLineStartToFile contents)
-- example ghci> hsToLhs "helloworld.hs" "helloworld.lhs" 
-- hs = input file
-- lhs = output file

main :: IO ()
main = do  [hs, lhs] <- getArgs
           hsToLhs hs lhs

-- 3 NUMBER GUESSING GAME -----------------------------------------------------------------------------

-- a) playRound number
-- ask a number to user until they guess the number
playRound :: Int -> IO ()
playRound n = do 
    putStrLn "Guess a number"
    guess <- getInt
    if guess == n 
        then putStrLn "You got it right!"
        else do 
            if guess > n 
                then putStrLn "Too high"
                else putStrLn "Too low"
            playRound n
--example ghci> playRound 5

--b) dice n rolls an n-sided dice: first it generates a random number and then constrains it using the
--mod :: Int -> Int -> Int function to be between 0 and n − 1 inclusive
dice :: Int -> IO Int
dice n = do  
      i <- randomIO
      return (mod i n)
-- example ghci> dice 6

-- c) 
game :: Int -> IO ()
game difficulty = do  
      number <- dice difficulty
      playRound number
-- example ghci> game 6








