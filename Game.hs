module Game where

--vrijednost polja: broj susjednih mina - od 0 do 8 ili mina
data FieldValue = Neighbours Int
 | Mine
 deriving (Eq,Show)

--stanje polja - neotkriveno, kliknuto (vrijednost je broj od 0-8 ili mina) ili postavljena zastavica
data FieldState = Uncovered
 | Clicked FieldValue
 | Flag
 | FalseFlag --polja koja su pogresno oznacena zastavicom
 deriving (Eq,Show)

--tip koji predstavlja stanje igre
data GameState = Game ([FieldValue], [FieldState])
 | GameOver ([FieldValue], [FieldState])
 | Win ([FieldValue], [FieldState])
 deriving Show
