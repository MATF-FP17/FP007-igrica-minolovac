module Main where

import Random(create_random_list)
import Mines(generateMinesMatrix,generateInitialState)
import Drawing(render)
import Game
import Play(handleKeys)
import Window
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Data.Time.Clock.POSIX

mainWindow :: Display
mainWindow = InWindow "Minesweeper" (size,size) (position,position)

background :: Color
background = cyan

--broj frejmova u sekundi
fps :: Int
fps = 60

--broj mina u igri je mines_num
mines_num :: Int
mines_num = (fields_num `div` 8) * (fields_num `div` 8) * 10

main :: IO()
main = do
         --uzimamo trenutno vrijeme
         current_time <- fmap round getPOSIXTime
         brojevi <- mapM (\x -> loadBMP $ "76px-Minesweeper_" ++ show x ++ "_svg.bmp") [0,1..8]
         mina <- loadBMP "1473847111_large.bmp"
         zastavica <- loadBMP "76px-Minesweeper_flag_svg.bmp"
         precrtano <- loadBMP "1473847111_large_crossed.bmp"
         let random_list = create_random_list fields_num current_time
             slike = brojevi ++ [mina,zastavica,precrtano]
             mines_matrix = generateMinesMatrix fields_num mines_num random_list
             initialState = generateInitialState mines_matrix fields_num mines_num
         play mainWindow background fps initialState (render slike) handleKeys update
           where
             update :: Float -> GameState -> GameState
             update _ = id
