import Data.List

--WRITEFILE => does not write the file. only computes the instruction to write

-- Functions => always give the same result for the same arguments. (PURE)
-- Instructions => can behave differently on different occasions (IMPURE)

-- putting insutrcution together 
writeTwoFiles :: FilesPath -> String -> IO ()
writeTwoFiles file s = 
  do writeFile (file ++ "1") s
     writeFile (file ++ "2") s


main = do
       putStrLn "Greetings!  What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"

-- read and write to the file
copyFile :: FilePath -> FilePath -> IO ()
copyFile from to =
  do contents <- readFile from
     writeFile to contents

-- 
catFiles :: FilePath -> FilePath -> IO String
catFiles file1 file2 =
  do s1 <- readFile file1
     s2 <- readFile file2
     return (s1++s2) -- use return to create an instruction with just a result

-- doTwice :: IO a -> IO (a,a)
doTwice io =
  do x <- io
     y <- io
     return (x,y)

dont :: IO a -> IO ()
dont io =
  do return ()

second :: [IO a] -> IO a
second (_:io:_) = io

sortFile :: FilePath -> FilePath -> IO ()
sortFile file1 file2 =
  do s <- readFile file1
     writeFile file2 (unlines (sort (lines s)))

getLine' :: IO String
getLine' =
  do c <- getChar
     if c == '\n'
       then return ""
       else do cs <- getLine'
               return (c:cs)

writeFiles :: FilePath -> [String] -> IO ()
writeFiles file xs =
  sequence_ [ writeFile (file++show i) x
            | (x,i) <- zip xs [1..length xs]
            ]

writeFiles' :: FilePath -> [String] -> IO ()
writeFiles' file xs =
  sequence_ [ writeFile (file++show i) x
            | x <- xs, i <- [1..length xs]
            ]

sequence_' :: [IO ()] -> IO ()
sequence_' [] = return ()
sequence_' (io:ios) =
  do io
     sequence_' ios

sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (io:ios) =
  do a  <- io
     as <- sequence' ios
     return (a:as)

