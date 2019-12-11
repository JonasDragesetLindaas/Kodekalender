import System.IO 
import Data.List

data Day        = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)
type Report     = (Day, Int, Int, Int) -- Day, Shampoo (Sjampo), Toothpaste (Tannkrem), Toilet paper (Toalettpapir)
type Data       = (String, Int)

firstDay :: Day
firstDay = Monday

nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

stripExtra :: [String] -> [String]
stripExtra []       = []
stripExtra (x:xs)   = (filter (/= '\t') x) : stripExtra xs

divide :: [String] -> Day -> [(Day, [Data])]
divide [] _              = []
divide (d:x:y:z:xs) day  = (day, a:b:c:[]) : divide xs (nextDay day) where  a = transformString x 
                                                                            b = transformString y
                                                                            c = transformString z   

sortData :: [(Day, [Data])] -> [(Day, [Data])]
sortData []     = []
sortData (x:xs) = (day, sorted) : sortData xs where day     = fst x
                                                    sorted  = sort . snd $ x

transformToReport :: [(Day, [Data])] -> [Report]
transformToReport []        = []
transformToReport (x:xs)    = (day, a, b, c) : transformToReport xs where   day = fst x
                                                                            a = snd ((snd x)!!0)
                                                                            b = snd ((snd x)!!1)
                                                                            c = snd ((snd x)!!2)

transformString :: String -> Data
transformString str = (s, n) where  s = last . words $ str
                                    n = read . head . tail . words $ str

process :: String -> [Report]
process str = transformToReport . sortData $ (divide (stripExtra . lines $str) firstDay)

answer :: [Report] -> (Int, [Int])
answer rep = (n, ls) where  ls = answer' rep 0 0 0 0 0
                            n = product ls

answer' :: [Report] -> Int -> Int -> Int -> Int -> Int -> [Int]
answer' [] a b c d e        = (div a 125):(div b 300):(div c 25):d:e:[]
answer' (x:xs) a b c d e    = answer' xs (a+ad) (b+bd) (c+cd) (d+dd) (e+ed) where   (day, bd, ad, cd) = x
                                                                                    dd = if day == Sunday then bd else 0
                                                                                    ed = if day == Wednesday then cd else 0

main = do 
    handle <- openFile "logg.txt" ReadMode
    contents <- hGetContents handle
    putStrLn (show (answer . process $ contents))