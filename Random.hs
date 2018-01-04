module Random where

import System.Random

--funkcija koja vraca listu od n random brojeva i generator
--koristimo trenutno vrijeme za randomizaciju
--poziva se sa make_rs n (fmap mkStdGen current_time)
make_rs :: RandomGen g => Int -> g -> ([Int],g)
make_rs n g = loop [] n g
  where
    loop acc 0 g = (reverse acc,g)
    loop acc n g = let (r,g') = randomR (0,n) g 
                   in loop (r:acc) (pred n) g'

--sada u ovoj listi samo treba uzeti indekse nekog broja od 0 do 5 koji se pojavljuje bar mines_num puta
--i ti indeksi ce biti pozicije mina na osnovu kojih treba ispravno napraviti funkciju generateInitialState
create_random_list :: Int -> Int -> [Int]
create_random_list n time = map (\x -> x `mod` 6) $ fst $ make_rs (n*n) $ mkStdGen time
