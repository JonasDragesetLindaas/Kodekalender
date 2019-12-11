rest :: Int -> Int
rest b  = b `rem` 27644437

kalkulerB :: Int -> Int -> Int
kalkulerB y' x  = y' * x

sjekk :: Int -> Int -> Bool
sjekk y x   = if rest (kalkulerB y x) == 1 then True else False

emul :: Int -> Int -> Int
emul y x    = if sjekk y x then y else emul (y+1) x

brute :: Int -> Int
brute x = emul 2 x

kalkulerZ :: Int -> Int 
kalkulerZ y = 5897 * y

los :: Int -> Int
los dag = rest (kalkulerZ (brute dag))