import System.IO

data TerrainType    = Gress | Is | Asfalt | Skog | Fjell deriving Show
type TerrainUnit    = (TerrainType, Int)    -- Represents a "set" of terrain withe the type and how many successive tiles there are
type Speed          = Int

initialSpeed :: Int 
initialSpeed = 10703437

updateSpeed :: Speed -> TerrainUnit -> Speed
updateSpeed s (Gress, n)    = s - (27 * n)
updateSpeed s (Is, n)       = s + (sum (map (*12) [1..n]))
updateSpeed s (Asfalt, n)   = s - (59 * n)
updateSpeed s (Skog, n)     = s - (212 * n)
updateSpeed s (Fjell, n)    = s - (35 * (n `div` 2))

slowSled :: String -> Int 
slowSled inp    = slowSled' (processInput inp) initialSpeed

slowSled' :: [TerrainUnit] -> Speed -> Int
slowSled' [] _      = 0
slowSled' (x:xs) n  = if n > 0 then (snd x) + slowSled' xs (updateSpeed n x) else 0

processInput :: String -> [TerrainUnit]
processInput []     = []
processInput (x:xs) = let (n, ls) = consumeTerrain (x:xs) in case x of  'G' -> (Gress, n)   : processInput ls
                                                                        'I' -> (Is, n)      : processInput ls
                                                                        'A' -> (Asfalt, n)  : processInput ls 
                                                                        'S' -> (Skog, n)    : processInput ls
                                                                        'F' -> (Fjell, n)   : processInput ls

consumeTerrain :: String -> (Int, String)
consumeTerrain inp  = consumeTerrain' inp (head inp) 0

consumeTerrain' :: String -> Char -> Int -> (Int, String)
consumeTerrain' [] _ n      = (n, "") 
consumeTerrain' (x:xs) t n  = if x == t then consumeTerrain' xs t (n+1) else (n, x:xs)


--Was bored, so I made these for fun, not very good, but they work
checkOccurences :: String -> (Int, Int, Int, Int, Int)
checkOccurences inp = checkOccurences' inp 0 0 0 0 0 

checkOccurences' :: String -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int, Int)
checkOccurences' [] g i a s f       = (g, i, a, s, f)
checkOccurences' (x:xs) g i a s f   = case x of 'G' -> checkOccurences' xs (g+1) i a s f
                                                'I' -> checkOccurences' xs g (i+1) a s f
                                                'A' -> checkOccurences' xs g i (a+1) s f
                                                'S' -> checkOccurences' xs g i a (s+1) f
                                                'F' -> checkOccurences' xs g i a s (f+1)

main2 = do 
    handle <- openFile "terreng.txt" ReadMode
    contents <- hGetContents handle 
    putStrLn (show . checkOccurences $ contents)

main = do 
    handle <- openFile "terreng.txt" ReadMode
    contents <- hGetContents handle
    putStrLn (show . slowSled $ contents)