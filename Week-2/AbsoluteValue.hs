{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

absolute :: Int -> Int  
absolute x | x >= 0 = x
absolute x | x < 0 = -x 

-- same as above
absolute x | x >= 0 = x
           | x < 0 = -x 

