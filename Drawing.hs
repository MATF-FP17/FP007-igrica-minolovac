module Drawing where

import Game
import Window
import Graphics.Gloss

draw_rect :: [Picture] -> (FieldValue, FieldState) -> Float -> Float -> Float -> Float -> Picture
draw_rect pics (value, Uncovered) x_begin x_end y_begin y_end = polygon [(x_begin, y_begin), (x_end, y_begin), (x_end, y_end), (x_begin, y_end)]
draw_rect pics (value, Clicked Mine) x_begin x_end y_begin y_end = translate (x_begin + half_field_size) (y_begin + half_field_size) $ scale 0.875 0.875 $ pics !! 9
draw_rect pics (value, Clicked (Neighbours x)) x_begin x_end y_begin y_end = translate (x_begin + half_field_size) (y_begin + half_field_size) $ scale 0.875 0.875 $ pics !! x
draw_rect pics (value, Flag) x_begin x_end y_begin y_end = translate (x_begin + half_field_size) (y_begin + half_field_size) $ scale 0.875 0.875 $ pics !! 10
draw_rect pics (value, FalseFlag) x_begin x_end y_begin y_end = translate (x_begin + half_field_size) (y_begin + half_field_size) $ scale 0.875 0.875 $ pics !! 11

coordinates = map fromIntegral [(-size) `div` 2, (-size) `div` 2 + size `div` fields_num ..]
begin_coordinates = take fields_num coordinates
end_coordinates = take fields_num (tail coordinates)

--odredjuje redni broj reda ili kolone za proslijedjenu desnu x ili gornju y koordinatu
row_num :: Float -> Float
row_num y = fromIntegral $ fields_num - (size `div` 2 + (round y)) `div` (size `div` fields_num)

column_num :: Float -> Float
column_num y = (fromIntegral $ fields_num-1) - (row_num y)

--Na osnovu pozicije od 0 do 7 u koloni vraca bas taj element
takeElemValueState :: [FieldValue] -> [FieldState] -> Float -> (FieldValue, FieldState)
takeElemValueState values states col_num = (values !! (round col_num), states !! (round col_num))

draw_rect_row :: [Picture] -> ([FieldValue], [FieldState]) -> Float -> Float -> [Picture]
draw_rect_row pics pair_game y_begin y_end = map (\tuple_x -> draw_rect pics (takeElemValueState (fst pair_game) (snd pair_game) (column_num $ snd   tuple_x)) (fst tuple_x) (snd tuple_x) y_begin y_end) $ zip begin_coordinates end_coordinates  

v_lines = map (color white) $ map (\x -> line[(x, fromIntegral $ (-size) `div` 2),(x, fromIntegral $ size `div` 2)]) begin_coordinates
h_lines = map (color white) $ map (\y -> line[(fromIntegral $ (-size) `div` 2, y),(fromIntegral $ size `div` 2, y)]) begin_coordinates

--ova funkcija iz listi vrijednosti i stanja svih polja uzima samo vrijednosti za svoj red koji je 3. argument i
--imace vrijednost od 0 do 7 
takeRowValuesStates :: [FieldValue] -> [FieldState] -> Float -> ([FieldValue], [FieldState])
takeRowValuesStates values states row_number =  
 let row_values = take fields_num $ drop (round row_number*fields_num) values
     row_states = take fields_num $ drop (round row_number*fields_num) states
 in (row_values, row_states)

--funkcija render sluzi da trenutno stanje igre pretvori u sliku koja ce nam se prikazati
--kao prvi argument prima niz slika koje ce se prikazivati, a koje se ucitaju u IO delu programa, a kao drugi trenutno stanje igre
render :: [Picture] -> GameState -> Picture

render pics (Game pair_game) = 
 let values = fst pair_game
     states = snd pair_game
 in pictures $ (concat $ map (\tuple_y -> draw_rect_row pics (takeRowValuesStates values states $ row_num $ snd tuple_y) (fst tuple_y) (snd tuple_y)) 
  $ zip begin_coordinates end_coordinates)
  ++ v_lines 
  ++ h_lines

render pics (GameOver pair_game) = 
 let values = fst pair_game
     states = snd pair_game
 in pictures $ (concat $ map (\tuple_y -> draw_rect_row pics (takeRowValuesStates values states $ row_num $ snd tuple_y) (fst tuple_y) (snd tuple_y)) 
  $ zip begin_coordinates end_coordinates)
  ++ v_lines 
  ++ h_lines

render pics (Win pair_game) = 
 let values = fst pair_game
     states = snd pair_game
 in pictures $ (concat $ map (\tuple_y -> draw_rect_row pics (takeRowValuesStates values states $ row_num $ snd tuple_y) (fst tuple_y) (snd tuple_y)) 
  $ zip begin_coordinates end_coordinates)
  ++ v_lines 
  ++ h_lines
