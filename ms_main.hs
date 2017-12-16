module Main where

import Graphics.Gloss

--size, fields_num, mines_num ce biti stepeni broja 2

--dimenzije prozora ce biti size x size 
size = 512

--pozicija prozora
position = 0

--igra ce imate fields_num x fields_num polja
fields_num = 8

--broj mina u igri je mines_num
mines_num = 8

mainWindow :: Display
mainWindow = InWindow "Minesweeper" (size,size) (position,position)

background :: Color
background = cyan

--draw_rect :: Float -> Float -> Float -> Float -> Picture
draw_rect x_begin x_end y_begin y_end = polygon [(x_begin, y_begin), (x_end, y_begin), (x_end, y_end), (x_begin, y_end)]

coordinates = [(-size) `div` 2, (-size) `div` 2 + size `div` fields_num ..]

begin_coordinates = take fields_num coordinates
end_coordinates = take fields_num (tail coordinates)

draw_rect_row y_begin y_end = map (\tuple_x -> draw_rect (fromIntegral $ fst tuple_x) (fromIntegral $ snd tuple_x) y_begin y_end) 
 $ zip begin_coordinates $ end_coordinates  

drawing = pictures $ concat $ map (\tuple_y -> draw_rect_row (fromIntegral $ fst tuple_y) (fromIntegral $ snd tuple_y)) 
 $ zip begin_coordinates $ end_coordinates


main :: IO()

main = display mainWindow background drawing 


