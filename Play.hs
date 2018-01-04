module Play where

import Game
import Window
import Graphics.Gloss.Interface.Pure.Game

--ova funkcija treba da promijeni stanje igre na osnovu polja na koje je korisnik kliknuo (polje je dato indeksima)
klikniPolje :: (Int, Int) -> GameState -> GameState
klikniPolje (i,j) oldGame@(Game (values,states)) =
  let states' = take (fields_num*i+j) states             --pravim novu listu stanja polja, ovo je deo pre kliknutog polja
      states'' = drop (fields_num*i+j+1) states          --ovo je deo posle kliknutog polja
      oldState = states !! (fields_num*i+j)              --potrebno je pamtiti staro stanje, zbog izlaska iz rekurzije (ovde se gleda indeks, a indeksi se broje od 0)
      newState = Clicked (values !! (i*fields_num+j))    --ovo je novo stanje kliknutog polja
      indeksi_suseda = [(i+s,j+t) | s <- [-1,0,1], t <- [-1,0,1], i+s>=0, j+t>=0, i+s<fields_num, j+t<fields_num, s^2+t^2>0]
      newGame = Game (values, states' ++ (newState:states''))
  in if(i<0 || j<0 || i>(fields_num-1) || j>(fields_num-1))
     then oldGame
     else if (oldState == Uncovered)                          --stanje igre menjamo samo ako je kliknuto polje bilo nekliknuto do tada
               then                     
               if (newState == Clicked Mine)
                 then lose values states
                 else if (newState == Clicked (Neighbours 0))    --ako je kliknuto polje u cijoj okolini nema mina, treba kliknuta sva okolna polja
                        then foldl (\game (a,b) -> klikniPolje (a,b) game) newGame indeksi_suseda
                        else checkWin newGame
               else oldGame

--ako igra nije u toku, nemamo akciju
klikniPolje _ x = x

--funkcija koja vrsi pripreme za prikaz izgubljene igre
lose :: [FieldValue] -> [FieldState] -> GameState
lose values states = let clickMines :: (FieldValue, FieldState) -> (FieldValue, FieldState)
                         clickMines (Mine,Flag) = (Mine,Flag)
                         clickMines (Mine,_) = (Mine,Clicked Mine) --sva polja na kojima su mine treba da dobiju stanje kliknute mine
                         clickMines (x,Flag) = (x,FalseFlag) -- sva polja na kojima su zastavice, a na kojima nisu mine, treba da dobiju stanje pogresne zastave (FalseFlag)
                         clickMines x = x
                         gameOver = map clickMines $ zip values states
                     in GameOver (map fst gameOver, map snd gameOver)

--funkcija koja proverava da li je ostvarena pobeda
checkWin :: GameState -> GameState
checkWin game@(Game (values,states)) = let checkMines :: (FieldValue, FieldState) -> Bool
                                           checkMines ((Neighbours x),(Clicked (Neighbours y))) = x == y --ovo je uvek tacno, ne utice na ukupnu tacnost
                                           checkMines ((Neighbours x),_) = False --ako polje koje nije mina nije kliknuti, igra nije dobijena
                                           checkMines _ = True
                                       in if and $ map checkMines $ zip values states --proverava se da li ne postoji nijedno polje koje nije mina i nije kliknuto
                                            then win values states --ako ne postoji, onda vrsimo pripreme za prikay pobede
                                            else game --inace samo prosledjujemo igru kakva je poslata ovoj funkciji
checkWin x = x

--funkcija koja vrsi pripreme za prikaz dobijene igre
win :: [FieldValue] -> [FieldState] -> GameState
win values states = let flagMines :: (FieldValue, FieldState) -> (FieldValue, FieldState)
                        flagMines (Mine,_) = (Mine,Flag) --polja na kojima su mine treba da dobiju zastavicu
                        flagMines x = x --ostala polja ostaju u stanju u kojem su (polja koja nisu mine treba da budu kliknuta)
                        game = map flagMines $ zip values states
                     in Win (map fst game, map snd game)

--funkcija koja pretvara koordinate misa u indekse polje i poziva funkciju klikniPolje
nextState :: Float -> Float -> GameState -> GameState
nextState x y game = let a = size `div` 2
                         b = size `div` fields_num
                         i = floor $ ((fromIntegral a)-y)/(fromIntegral b)
                         j = floor $ (x+(fromIntegral a))/(fromIntegral b)
                     in klikniPolje (i,j) game

--funkcija koja pretvara koordinate misa u indekse polja i postavlja ili skida zastavicu
flag :: Float -> Float -> GameState -> GameState

--postavlja, odnosno skida zastavicu samo ako je igra u toku
flag x y oldGame@(Game (values,states)) = let a = size `div` 2
                                              b = size `div` fields_num
                                              i = floor $ ((fromIntegral a)-y)/(fromIntegral b)
                                              j = floor $ (x+(fromIntegral a))/(fromIntegral b)
                                              states' = take (fields_num*i+j) states             --pravim novu listu stanja polja, ovo je deo pre kliknutog polja
                                              states'' = drop (fields_num*i+j+1) states          --ovo je deo posle kliknutog polja
                                              oldState = states !! (fields_num*i+j)
                                          in if (oldState == Uncovered)
                                               then Game (values,states' ++ (Flag:states''))
                                               else if (oldState == Flag)
                                                      then Game (values,states' ++ (Uncovered:states''))
                                                      else oldGame

--u ostalim slucajevima funkcija ne radi nista
flag _ _ x = x

--funkcija koja reaguje na dogadjaj (samo na klik misem, za sada)
handleKeys :: Event -> GameState -> GameState

--reakcija na levi klik (stavio sam Down da bih mogao lakse da testiram)
handleKeys (EventKey (MouseButton LeftButton) Down _ (x,y)) game = nextState x y game

--reakcija na desni klik
handleKeys (EventKey (MouseButton RightButton) Down _ (x,y)) game = flag x y game

--na ostale dogadjaje ne reaguje
handleKeys _ game = game
