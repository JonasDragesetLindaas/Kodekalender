import System.IO 

kuttBytt :: String -> String
kuttBytt inp = b ++ a where (a,b) = halve inp

halve :: [a] -> ([a], [a]) 
halve xs = 
    ((take s xs), (drop s xs))
    where
        s = (length xs ) `div` 2

byttEnOgTo :: String -> String
byttEnOgTo []       = []
byttEnOgTo (a:b:ls) = b : a : byttEnOgTo ls

byttSisteOgForsteTre :: String -> String
byttSisteOgForsteTre []     = []
byttSisteOgForsteTre inp    = b ++ byttSisteOgForsteTre liste ++ a where    
                                                                    a = take 3 inp
                                                                    b = reverse (take 3 (reverse inp))
                                                                    liste = treGanger tail (treGanger init inp)

treGanger :: (a -> a) -> a -> a
treGanger f = f . f . f

enkod :: String -> String
enkod inp = byttSisteOgForsteTre (byttEnOgTo (kuttBytt inp))

dekod :: String -> String 
dekod inp = kuttBytt (byttEnOgTo (byttSisteOgForsteTre inp))