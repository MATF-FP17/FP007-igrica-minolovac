module Mines where

import Game
import Data.List

generateMinesMatrix :: Int -> Int -> [Int] -> [Bool]
generateMinesMatrix fields_n mines_n r_list =
  let
    --sljedeca linija pronalazi najmanji element od 0 do 5 sa frekvencijom bar mines_num, indeksi u random_list tog elementa bice pozicije mina
    --treba pojednostaviti sljedeci kod
    selectedElem = ( fst . head . dropWhile (\g -> snd g < mines_n) . map (\g -> (head g, length g)). group . sort) r_list

    --TODO
    --Uraditi ovo sa foldl
    minesBefore :: Int -> [(Bool, Int)] -> [(Bool, Int)]
    minesBefore _ []     = []
    minesBefore acc (x:xs) = if fst x == True then (fst x,acc+1+snd x) : minesBefore (acc+1) xs
                                              else (fst x, acc) : minesBefore acc xs

    matrix_many_mines = map (\x -> x == selectedElem) r_list

    minesBefore10 = minesBefore 0 $ zip matrix_many_mines (repeat 0)
    firstPart = map fst $ takeWhile (\x -> snd x < 10) $ minesBefore10
    lengthTaken = length firstPart
  in take (lengthTaken+1) matrix_many_mines ++ take ((fields_n*fields_n)-lengthTaken-1) (repeat False)

-- ova funkcija ce na osnovu generisanih mina postaviti ispravne brojeve od 0 do 8 na sva ostala polja
count_mines :: Int -> [FieldValue] -> [FieldValue]
count_mines fields_n x =
  let indeksi = map (\a -> (a `div` fields_n, a `mod` fields_n)) [0,1..]
      polja = zip indeksi x
      count :: ((Int,Int),FieldValue) -> FieldValue
      count (_,Mine) = Mine
      count ((i,j),_) = let indeksi_suseda = [(i+s,j+t) | s <- [-1,0,1], t <- [-1,0,1], i+s>=0, j+t>=0, i+s<fields_n, j+t<fields_n, s^2+t^2>0]
                            broj_mina = length $ filter (Mine ==) [snd $ polja !! (fields_n*a+b) | (a,b) <- indeksi_suseda]
                        in Neighbours broj_mina
  in map count polja

--funkcija koja na pocetku generise igru, ona treba da postavi mine, pa postavi brojeve od 0-8 u ostala polja
--ona ce da postavi sva polja matrice za stanje igre na Uncovered
generateInitialState :: [Bool] -> Int -> Int -> GameState
generateInitialState m_matrix fields_n mines_n =
  let fields_value = count_mines fields_n $ map (\x -> if fst x then Mine else Neighbours 0) $ zip m_matrix [0..]
      fields_state = map (\_ -> Uncovered) fields_value 
  in Game (fields_value, fields_state)
