import System.IO 

fyll :: String -> Int 
fyll (x:xs)     = fyll' (x:xs) (x == '#') 0

fyll' :: String -> Bool -> Int -> Int
fyll' [] _ _            = 0
fyll' (x:xs) frstNo tmp = case x of '#' -> if frstNo then tmp + fyll' xs frstNo 0 else fyll' xs True 0
                                    ' ' -> fyll' xs frstNo (tmp + 1)
                            

syndefloden :: [String] -> [Int] 
syndefloden []  = []
syndefloden inp = fyll (last inp) : syndefloden (init inp)

syndefloden' :: [String] -> Int 
syndefloden' inp    = sum (syndefloden inp)

main = do 
    handle <- openFile "world.txt" ReadMode
    contents <- hGetContents handle
    putStrLn (show (syndefloden' (lines contents)))