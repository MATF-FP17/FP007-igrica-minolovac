module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Game
--import Data.List.Split

--size, fields_num, mines_num ce biti stepeni broja 2
--dimenzije prozora ce biti size x size 
size = 512

--pozicija prozora
position = 0

--igra ce imate fields_num x fields_num polja
fields_num = 8

--broj mina u igri je mines_num
mines_num = 10 

mainWindow :: Display
mainWindow = InWindow "Minesweeper" (size,size) (position,position)

background :: Color
background = cyan

--vrijednost polja: broj susjednih mina - od 0 do 8 ili mina
data FieldValue = Neighbours Int
 | Mine
 deriving (Eq,Show)

--stanje polja - neotkriveno, kliknuto (vrijednost je broj od 0-8 ili mina) ili postavljena zastavica
data FieldState = Uncovered
 | Clicked FieldValue
 | Flag
 deriving (Eq,Show)

--tip koji predstavlja stanje igre
data GameState = Game ([FieldValue], [FieldState])
 | GameOver     
 deriving Show

--ovde bi trebalo da budu: matrica kliknutih polja, matrica mina i mozda jos ponesto...
initialState :: GameState
initialState = generateInitialState fields_num mines_num

-- ova funkcija ce na osnovu generisanih mina postaviti ispravne brojeve od 0 do 8 na sva ostala polja
count_mines :: [FieldValue] -> [FieldValue]
count_mines x =
  let indeksi = map (\a -> (a `div` fields_num, a `mod` fields_num)) [0,1..]
      polja = zip indeksi x
      count :: ((Int,Int),FieldValue) -> FieldValue
      count (_,Mine) = Mine
      count ((i,j),_) = let indeksi_suseda = [(i+s,j+t) | s <- [-1,0,1], t <- [-1,0,1], i+s>=0, j+t>=0, i+s<fields_num, j+t<fields_num, s^2+t^2>0]
                            broj_mina = length $ filter (Mine ==) [snd $ polja !! (fields_num*a+b) | (a,b) <- indeksi_suseda]
                        in Neighbours broj_mina
  in map count polja

--funkcija koja na pocetku generise igru, ona treba da postavi mine, pa postavi brojeve od 0-8 u ostala polja
--ona ce da postavi sva polja matrice za stanje igre na Uncovered

generateInitialState :: Int -> Int -> GameState
--generateInitialState fields_n mines_n = 
 --let fields_value = count_mines $ map (\x -> if (fst x) `mod` fields_n == 0 then Mine else Neighbours 0) $ zip [0,1..] $ take (fields_n*fields_n) $ map (\x -> Neighbours 0) [1..]
     --fields_state = map (\x -> Uncovered) fields_value   
 --in Game (Data.List.Split.chunksOf fields_n fields_value,Data.List.Split.chunksOf fields_n fields_state)
generateInitialState fields_n mines_n =
   let fields_value = count_mines $ take (fields_n*fields_n) $ map (\x -> if (fst x) `mod` fields_n == 0 then Mine else Neighbours 0) $ zip [0,1..] [1..]
       fields_state = map (\x -> Uncovered) fields_value 
   in Game (fields_value, fields_state)

--broj frejmova u sekundi
fps :: Int
fps = 60

--ova funkcija treba da promijeni stanje igre na osnovu polja na koje je korisnik kliknuo (polje je dato indeksima)
klikniPolje :: (Int, Int) -> GameState -> GameState
klikniPolje (i,j) (Game (values,states)) = let states' = take (fields_num*i+j-1) states           --pravim novu listu stanja polja, ovo je deo pre kliknutog polja
                                               states'' = drop (fields_num*i+j) states            --ovo je deo posle kliknutog polja
                                               oldState = states !! (fields_num*i+j)              --potrebno je pamtiti staro stanje, zbog izlaska iz rekurzije
                                               newState = Clicked (values !! (i*fields_num+j))    --ovo je novo stanje kliknutog polja
                                               indeksi_suseda = [(i+s,j+t) | s <- [-1,0,1], t <- [-1,0,1], i+s>=0, j+t>=0, i+s<fields_num, j+t<fields_num, s^2+t^2>0]
                                               newGame = Game (values, states' ++ (newState:states''))
                                           in if (newState == Clicked Mine) then GameOver
                                                --ako je kliknuto polje u cijoj okolini nema mina, treba kliknuta sva okolna polja ukoliko nisu vec kliknuta
                                                --trenutno nije dobra implementacija, treba popraviti
                                                --else if and [oldState == Uncovered, newState == Clicked (Neighbours 0)]
                                                       --then foldl (\game (a,b) -> klikniPolje (a,b) game) newGame indeksi_suseda
                                                       else newGame

klikniPolje _ GameOver = GameOver

--funkcija koja pretvara koordinate misa u indekse polje i poziva funkciju klikniPolje
nextState :: Float -> Float -> GameState -> GameState
nextState x y game = let a = size `div` 2
                         b = size `div` fields_num
                         i = floor $ (x+(fromIntegral a))/(fromIntegral b)
                         j = floor $ (y+(fromIntegral a))/(fromIntegral b)
                     in klikniPolje (i,j) game

--funkcija koja reaguje na dogadjaj (samo na klik misem, za sada)
handleKeys :: Event -> GameState -> GameState

--reakcija na levi klik
handleKeys (EventKey (MouseButton LeftButton) _ _ (x,y)) game = nextState x y game

--na ostale dogadjaje ne reaguje
handleKeys _ game = game

--IMPLEMENTIRATI
draw_rect :: (FieldValue, FieldState) -> Float -> Float -> Float -> Float -> Picture
draw_rect pair_vs x_begin x_end y_begin y_end = 
 let value = fst pair_vs
     state = snd pair_vs 
 in if state == Uncovered then polygon [(x_begin, y_begin), (x_end, y_begin), (x_end, y_end), (x_begin, y_end)]
    else Text "1"

coordinates = map fromIntegral [(-size) `div` 2, (-size) `div` 2 + size `div` fields_num ..]
begin_coordinates = take fields_num coordinates
end_coordinates = take fields_num (tail coordinates)

--Na osnovu pozicije od 0 do 7 u koloni vraca bas taj element
takeElemValueState :: [FieldValue] -> [FieldState] -> Float -> (FieldValue, FieldState)
takeElemValueState values states column_num = (values !! (round column_num), states !! (round column_num))

draw_rect_row :: ([FieldValue], [FieldState]) -> Float -> Float -> [Picture]
draw_rect_row pair_game y_begin y_end = map (\tuple_x -> draw_rect (takeElemValueState (fst pair_game) (snd pair_game) (row_column_num $ snd tuple_x)) (fst tuple_x) (snd tuple_x) 
y_begin y_end) 
 $ zip begin_coordinates end_coordinates  

v_lines = map (color white) $ map (\x -> line[(x, fromIntegral $ (-size) `div` 2),(x, fromIntegral $ size `div` 2)]) begin_coordinates
h_lines = map (color white) $ map (\y -> line[(fromIntegral $ (-size) `div` 2, y),(fromIntegral $ size `div` 2, y)]) begin_coordinates


--ova funkcija iz listi vrijednosti i stanja svih polja uzima samo vrijednosti za svoj red koji je 3. argument i
--imace vrijednost od 0 do 7 
takeRowValuesStates :: [FieldValue] -> [FieldState] -> Float -> ([FieldValue], [FieldState])
takeRowValuesStates values states row_number =  
 let row_values = take fields_num $ drop (round row_number*fields_num) values
     row_states = take fields_num $ drop (round row_number*fields_num) states
 in (row_values, row_states)

--odredjuje redni broj reda ili kolone za proslijedjenu desnu x ili gornju y koordinatu
row_column_num :: Float -> Float
row_column_num y = fromIntegral $ fields_num - (size `div` 2 + (round y)) `div` (size `div` fields_num)

render :: GameState -> Picture


--test: ako je kraj igre, ne iscrtava se nista
render GameOver = pictures []

--normalno iscrtavanje u ostalim slucajevima
render (Game pair_game) = 
 let values = fst pair_game
     states = snd pair_game
 in pictures $ (concat $ map (\tuple_y -> draw_rect_row (takeRowValuesStates values states $ row_column_num $ snd tuple_y) (fst tuple_y) (snd tuple_y)) 
  $ zip begin_coordinates end_coordinates)
  ++ v_lines 
  ++ h_lines

--funkcija render sluzi da trenutno stanje igre pretvori u sliku koja ce nam se prikazati
--za sada, ona ne zavisi od stanja igre, ovo treba uraditi

main :: IO()
main = play mainWindow background fps initialState render handleKeys update
  where
    update :: Float -> GameState -> GameState
    update _ = id



