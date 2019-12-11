import System.IO 
import Prelude hiding (foldr, foldl)

erKrampus :: Int -> Bool
erKrampus 0 = False
erKrampus 1 = False
erKrampus 2 = False
erKrampus 3 = False
erKrampus n = kanBli (filter (\(x,y) -> x /= 0 && y /= 0) (konverter . lagSplit . show $ (n*n))) n

kanBli :: [(Int, Int)] -> Int -> Bool
kanBli ls x = if elem x list then True else False where list = map (\(x,y) -> x+y) ls

konverter :: [(String, String)] -> [(Int, Int)]
konverter []        = []
konverter (l:ls)    = (x, y) : konverter ls where   x = read (fst l) :: Int
                                                    y = read (snd l) :: Int

lagSplit :: String -> [(String, String)]
lagSplit ln = lagSplit' 1 ln

lagSplit' :: Int -> String -> [(String, String)]
lagSplit' n ln  = if n < ((length ln)-1) then (splitAt n ln ): lagSplit' (n+1) ln else (splitAt n ln) : []

summer :: [Int] -> Int
summer = foldl1 (+)

main = do 
    handle <- openFile "krampus.txt" ReadMode
    contents <- hGetContents handle
    putStrLn (show . summer $ (filter erKrampus (map read (lines contents))))