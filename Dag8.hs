data Op     = Pluss4 | Pluss101 | Minus9 | Minus1 | ReverserSiffer | Opp7 | GangeMSD | DeleMSD | Pluss1TilPar | Trekk1FraOdde | RoterPar | RoterOdde | RoterAlle | Stopp deriving Show
type Tall   = (Bool, [Int]) -- Bool: om tallet er negativt eller positivt, [Int] hvert tall er fra 0-9 og representerer et siffer i et mer komplekst tall
type Maskin = [[Op]]

startMaskin :: Maskin
startMaskin = [ [Pluss101, Opp7, Minus9, Pluss101], [Trekk1FraOdde, Minus1, Minus9, Pluss1TilPar], [Pluss1TilPar, Pluss4, Pluss101, Minus9],
                [Minus9, Pluss101, Trekk1FraOdde, Minus1], [RoterOdde, Minus1, Pluss4, RoterAlle], [GangeMSD, Pluss4, Minus9, Stopp], 
                [Minus1, Pluss4, Minus9, Pluss101], [Pluss1TilPar, Minus9, Trekk1FraOdde, DeleMSD], [Pluss101, ReverserSiffer, Minus1, RoterPar], [Pluss4, GangeMSD, ReverserSiffer, Minus9]]

fraTall :: Tall -> Int
fraTall (pos, siffer)   = if pos then fraTall' siffer else -(fraTall' siffer) 

fraTall' :: [Int] -> Int
fraTall' []     = 0
fraTall' (x:xs) = x * p + fraTall' xs where p = product (take (length (xs)) (repeat 10))

tilTall :: Int -> Tall
tilTall 0   = (True, [0])
tilTall n   = if (signum n) < 0 then (False, tilTall' (abs n)) else (True, tilTall' n)

tilTall' :: Int -> [Int]
tilTall' 0  = []
tilTall' x  = tilTall' (x `div` 10) ++ [x `mod` 10]

enkelTallMod :: Tall -> Int -> Tall
enkelTallMod tall i = let n = fraTall tall in tilTall (n + i) 

spill :: Op -> Tall -> Maskin -> [Tall] -- Probably not going to work, change the number before using hentNesteOp if it doesn't
spill Pluss4 tall mask          = nyTall : spill nyOp nyTall nyMask where (nyOp, nyMask) = hentNesteOp nyTall mask ; nyTall = enkelTallMod tall 4     
spill Pluss101 tall mask        = nyTall : spill nyOp nyTall nyMask where (nyOp, nyMask) = hentNesteOp nyTall mask ; nyTall = enkelTallMod tall 101  
spill Minus9 tall mask          = nyTall : spill nyOp nyTall nyMask where (nyOp, nyMask) = hentNesteOp nyTall mask ; nyTall = enkelTallMod tall (-9) 
spill Minus1 tall mask          = nyTall : spill nyOp nyTall nyMask where (nyOp, nyMask) = hentNesteOp nyTall mask ; nyTall = enkelTallMod tall (-1) 
spill ReverserSiffer tall mask  = nyTall : spill nyOp nyTall nyMask where (nyOp, nyMask) = hentNesteOp nyTall mask ; nyTall = reverserTall tall      
spill Opp7 tall mask            = nyTall : spill nyOp nyTall nyMask where (nyOp, nyMask) = hentNesteOp nyTall mask ; nyTall = enkelTallMod tall 4    
spill GangeMSD tall mask        = nyTall : spill nyOp nyTall nyMask where (nyOp, nyMask) = hentNesteOp nyTall mask ; nyTall = msd (*) tall           
spill DeleMSD tall mask         = nyTall : spill nyOp nyTall nyMask where (nyOp, nyMask) = hentNesteOp nyTall mask ; nyTall = msd div tall
spill Pluss1TilPar tall mask    = nyTall : spill nyOp nyTall nyMask where (nyOp, nyMask) = hentNesteOp nyTall mask ; nyTall = plussEnPar tall
spill Trekk1FraOdde tall mask   = nyTall : spill nyOp nyTall nyMask where (nyOp, nyMask) = hentNesteOp nyTall mask ; nyTall = trekkFraEnOdd tall
spill RoterPar tall mask        = tall : spill nyOp tall nyMask where (nyOp, nyMask) = hentNesteOp tall oldMask ; oldMask = (roterMaskin mask False True)
spill RoterOdde tall mask       = tall : spill nyOp tall nyMask where (nyOp, nyMask) = hentNesteOp tall oldMask ; oldMask = (roterMaskin mask False False)
spill RoterAlle tall mask       = tall : spill nyOp tall nyMask where (nyOp, nyMask) = hentNesteOp tall oldMask ; oldMask = (roterMaskin mask True True)
spill Stopp tall _              = [tall]

hentNesteOp :: Tall -> Maskin -> (Op, Maskin)
hentNesteOp (_, siffer) mask    = ((mask !! n !! 0), (roterEtHjul mask n)) where n = last siffer

reverserTall :: Tall -> Tall
reverserTall (pos, siffer)  = (pos, reverse (trimTall siffer))

trimTall :: [Int] -> [Int]
trimTall (n:ns) = if n == 0 then trimTall ns else (n:ns)

oppTilSyv :: Tall -> Tall
oppTilSyv tall  = let n = last (snd tall) in if n == 7 then tall else oppTilSyv (enkelTallMod tall 1)

msd :: (Int -> Int -> Int) -> Tall -> Tall
msd f tall  = let n = head (snd tall) in tilTall (f (fraTall tall) n)

plussEnPar :: Tall -> Tall
plussEnPar (pos, siffer)    = (pos, binarEndre True 1 siffer)

trekkFraEnOdd :: Tall -> Tall
trekkFraEnOdd (pos, siffer) = (pos, binarEndre False (-1) siffer)

binarEndre :: Bool -> Int -> [Int] -> [Int]
binarEndre _ _ []           = []
binarEndre erPar d (x:xs)   = if ((mod x 2) == 0) == erPar then (x+d) : binarEndre erPar d xs else x : binarEndre erPar d xs 

roterMaskin :: Maskin -> Bool -> Bool -> Maskin
roterMaskin mask rotAlle rotPar = case rotAlle of   True    -> map roterHjul mask
                                                    False   -> zipWith (\t -> \h -> if t then roterHjul h else h) alt mask where alt = annenhverBool rotPar

annenhverBool :: Bool -> [Bool]
annenhverBool start = if start then annenhverBool' odd else annenhverBool' even

annenhverBool' :: (Int -> Bool) -> [Bool]
annenhverBool' f    = [f x | x <- [1..]]

roterEtHjul :: Maskin -> Int -> Maskin
roterEtHjul (x:xs) 0    = roterHjul x : xs
roterEtHjul (x:xs) n    = x : roterEtHjul xs (n-1)

roterHjul :: [Op] -> [Op]
roterHjul (h:hs)    = hs ++ [h]

strt :: Int -> [Tall]
strt n  = spill (fst (hentNesteOp (tilTall n) startMaskin)) (tilTall n) startMaskin

start :: Int -> [Int]
start n = fraTallListe (strt n)

sim :: Int
sim = maximum (sim' 0)

sim' :: Int -> [Int]
sim' 11 = []
sim' n  = fraTall (last (strt n)) : sim' (n+1)

fraTallListe :: [Tall] -> [Int]
fraTallListe []     = []
fraTallListe (x:xs) = fraTall x : fraTallListe xs