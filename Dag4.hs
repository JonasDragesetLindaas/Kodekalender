import System.IO 

type Koord  = (Int, Int)
type Kart   = [[Int]]
type Stat   = (Kart, Koord)     --Tilsvarer et kart og sneglens posisjon på kartet

startPos :: Koord 
startPos = (0, 0)

initialiserKart :: Int -> Kart
initialiserKart n  = take n (repeat (take n (repeat 0)))

markerRute :: Kart -> Koord -> Kart
markerRute [] _         = []
markerRute (x:xs) (a,b) = if a == 0 then markerRute' x b : cont else x : cont where cont = markerRute xs (a-1,b)

markerRute' :: [Int] -> Int -> [Int]
markerRute' [] _        = []
markerRute' (x:xs) n    = if n == 0 then (x+1) : cont else x : cont where cont = markerRute' xs (n-1)

hentVerdi :: Kart -> Koord -> Int
hentVerdi kart (a, b)   = (kart!!a)!!b

flytt :: Stat -> Koord -> Int -> (Stat, Int)
flytt (kart, (x0, y0)) (x1, y1) n = if x0 /= x1
                                    then if x0 < x1 
                                        then flytt ((markerRute kart (x0, y0)), ((x0+1), y0)) (x1, y1) (n + 1 + (hentVerdi kart ((x0+1), y0)))
                                        else flytt ((markerRute kart (x0, y0)), ((x0-1), y0)) (x1, y1) (n + 1 + (hentVerdi kart ((x0-1), y0)))
                                    else if y0 /= y1
                                    then if y0 < y1
                                        then flytt ((markerRute kart (x0, y0)), (x0, (y0+1))) (x1, y1) (n + 1 + (hentVerdi kart (x0, (y0+1))))
                                        else flytt ((markerRute kart (x0, y0)), (x0, (y0-1))) (x1, y1) (n + 1 + (hentVerdi kart (x0, (y0-1))))
                                    else ((kart, (x0, y0)), n)


skattejakt :: Int -> [Koord] -> Int 
skattejakt n koordListe = skattejakt' ((initialiserKart n), (0, 0)) koordListe 0


skattejakt' :: Stat -> [Koord] -> Int -> Int
skattejakt' _ [] n          = n
skattejakt' stat (k:ks) n   = skattejakt' nyStat ks teller where (nyStat, teller) = flytt stat k n

----------------------------------------------------------------- IO stuff
forbered :: String -> [Koord]
forbered = konverter . split . map (\c -> if c=='\n' then ','; else c)

konverter :: [String] -> [Koord]
konverter []    = []
konverter inp   = (a,b) : konverter c where a = read (head inp) :: Int
                                            b = read (head (tail inp)) :: Int
                                            c = tail (tail inp)

split :: String -> [String]
split inp   = wordsWhen (==',') inp

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

main = do 
    handle <- openFile "coords.csv" ReadMode
    contents <- hGetContents handle
    putStrLn (show (skattejakt 1000 (forbered contents)))