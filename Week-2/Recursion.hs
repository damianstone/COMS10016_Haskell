{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

power :: Int -> Int -> Int 
power x 0 = 1 -- the base 
power x n | n > 0 = x * power x (n-1)

-- replicate a given Word 
repli :: Int -> String -> String  
repli 0 s = "" -- make the base as simple as possible
repli n s | n > 0 = s ++ repli (n-1) s   

