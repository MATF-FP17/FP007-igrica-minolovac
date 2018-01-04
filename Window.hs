module Window where

--size, fields_num, mines_num ce biti stepeni broja 2
--dimenzije prozora ce biti size x size 
field_size :: Int
field_size = 64

half_field_size :: Float
half_field_size = (fromIntegral field_size) / 2

size :: Int
size = field_size*fields_num

--pozicija prozora
position :: Int
position = 0

--igra ce imate fields_num x fields_num polja
fields_num :: Int
fields_num = 8
