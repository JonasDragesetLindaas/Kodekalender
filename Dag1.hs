import System.IO 

type Drage = (Int, Int) -- (Størrelse, dager dragen har vært sulten)
type Sau = Int          

-- Utfører en runde med "spising"
spis :: Drage -> Sau -> (Drage, Sau)
spis (a,b) sau  = if a > sau then ((a-1, b+1), 0) else ((a+1, 0), sau-a)

-- Sjekker om en drage er sulten nok til å spise folk 
drageSpiserFolk :: Drage -> Bool
drageSpiserFolk (a,b)   = if b >= 5 then True else False

-- Rekursiv traversering av input 
drager :: Int -> [Int] -> Drage -> Sau -> String
drager _ [] _ _                     = error "Gikk tom for input"
drager dag (x:xs) drage ekstraSauer = if (drageSpiserFolk drage) 
                                        then "Befolkningen overlevde i " ++ (show (dag-1)) ++ " dager" 
                                        else drager (dag+1) xs (fst spist) (snd spist)
                                            where spist = spis drage (x+ekstraSauer) 

transformer :: String -> [Int]
transformer inp = map read (words (filter (/= ',') inp) ) :: [Int]

main = do 
    handle <- openFile "sau.txt" ReadMode
    contents <- hGetContents handle
    let inp = transformer contents
    putStrLn (drager 0 inp (50,0) 0)