--Sente ARRIBA
--Gote ABAJO
import Data.Maybe

data ShogiPlayer = Sente | Gote deriving (Eq, Show, Enum) 

data Coordenada = Coordenada Int Int deriving (Eq, Show) 

data Pieza = Rey | Alfil | Torre | GeneralPlateado | GeneralDorado | Peon | Lancero | Caballo | Alfil2 | Torre2 | GeneralPlateado2 | Peon2 | Lancero2 | Caballo2 deriving (Eq, Show, Enum) 

data ShogiAction = Movimiento Coordenada Coordenada Bool | Arrojar Pieza Coordenada deriving (Eq, Show) 

data ShogiGame = ShogiGame ShogiPlayer [(Pieza, Coordenada, ShogiPlayer)] deriving (Eq, Show) 

beginning :: ShogiGame
beginning = ShogiGame Sente (peones++lanceros++caballos++generalesPlateados++generalesDorados++reyes++alfiles++torres)
	
peones :: [(Pieza,Coordenada,ShogiPlayer)]
peones = [ (Peon, (Coordenada x 3), Sente) |  x <- [1..9]]++[ (Peon, (Coordenada x 7), Gote) |  x <- [1..9]]

lanceros :: [(Pieza,Coordenada,ShogiPlayer)]
lanceros = [(Lancero, (Coordenada 9 9), Gote), (Lancero, (Coordenada 1 9), Gote), (Lancero, (Coordenada 1 1), Sente), (Lancero, (Coordenada 9 1), Sente)]

caballos :: [(Pieza,Coordenada,ShogiPlayer)]
caballos = [(Caballo, (Coordenada 8 9), Gote), (Caballo, (Coordenada 2 9), Gote), (Caballo, (Coordenada 2 1), Sente), (Caballo, (Coordenada 8 1), Sente)]

generalesPlateados :: [(Pieza,Coordenada,ShogiPlayer)]
generalesPlateados = [(GeneralPlateado, (Coordenada 7 9), Gote), (GeneralPlateado, (Coordenada 3 9), Gote), (GeneralPlateado, (Coordenada 3 1), Sente), (GeneralPlateado, (Coordenada 7 1), Sente)]

generalesDorados :: [(Pieza,Coordenada,ShogiPlayer)]
generalesDorados = [(GeneralDorado, (Coordenada 6 9), Gote), (GeneralDorado, (Coordenada 4 9), Gote), (GeneralDorado, (Coordenada 4 1), Sente), (GeneralDorado, (Coordenada 6 1), Sente)]

reyes :: [(Pieza,Coordenada,ShogiPlayer)]
reyes = [(Rey, (Coordenada 5 9), Gote), (Rey, (Coordenada 5 1), Sente)]

alfiles :: [(Pieza,Coordenada,ShogiPlayer)]
alfiles = [(Alfil, (Coordenada 8 8), Gote), (Alfil, (Coordenada 2 2), Sente)]

torres :: [(Pieza,Coordenada,ShogiPlayer)]
torres = [(Torre, (Coordenada 2 8), Gote), (Torre, (Coordenada 8 2), Sente)]


--activePlayer :: ShogiGame -> Maybe ShogiPlayer
--activePlayer (ShogiGame player _)  = if isFinished then Nothing else Just player

----Movimientos-----

esMovimientoPosibleGeneralDorado :: (Pieza, Coordenada, ShogiPlayer) -> [Coordenada]
esMovimientoPosibleGeneralDorado (p, (Coordenada x y), player) 
 |(player == Sente) && (y==9) && (x/=1) && (x/=9) = [(Coordenada (x+1) y)]++[(Coordenada (x-1) y)]++[(Coordenada x (y-1))]
 |(player == Sente) && (y==9) && (x==1) = [(Coordenada (x+1) y)]++[(Coordenada x (y-1))]
 |(player == Sente) && (y==9) && (x==9) = [(Coordenada (x-1) y)]++[(Coordenada x (y-1))]
 |(player == Sente) && (y==1) && (x/=1) && (x/=9) = [(Coordenada x (y+1))]++[(Coordenada (x+1) (y+1))]++[(Coordenada (x-1) (y+1))]++[(Coordenada (x+1) y)]++[(Coordenada (x-1) y)]
 |(player == Sente) && (y==1) && (x==1) = [(Coordenada x (y+1))]++[(Coordenada (x+1) (y+1))]++[(Coordenada (x+1) y)]
 |(player == Sente) && (y==1) && (x==9) = [(Coordenada x (y+1))]++[(Coordenada (x-1) (y+1))]++[(Coordenada (x-1) y)]
 |(player == Sente) && (x==9) && (y/=1) && (y/=9) = [(Coordenada x (y+1))]++[(Coordenada (x-1) (y+1))]++[(Coordenada (x-1) y)]++[(Coordenada x (y-1))]
 |(player == Sente) && (x==1) && (y/=1) && (y/=9) = [(Coordenada x (y+1))]++[(Coordenada (x+1) (y+1))]++[(Coordenada (x+1) y)]++[(Coordenada (x+1) (y-1))]
 |(player == Gote) && (y==9) && (x/=1) && (x/=9) = [(Coordenada x (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada x (y-1))]++[(Coordenada (x-1) y)]++[(Coordenada (x+1) y)]
 |(player == Gote) && (y==9) && (x==1) = [(Coordenada x (y-1))]++[(Coordenada (x+1) (y-1))]++[(Coordenada (x+1) y)]
 |(player == Gote) && (y==9) && (x==9) = [(Coordenada x (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x-1) y)]
 |(player == Gote) && (y==1) && (x/=1) && (x/=9) = [(Coordenada (x-1) y)]++[(Coordenada (x+1) y)]++[(Coordenada x (y+1))]
 |(player == Gote) && (y==1) && (x==1) = [(Coordenada x (y+1))]++[(Coordenada (x+1) y)]
 |(player == Gote) && (y==1) && (x==9) = [(Coordenada (x-1) y)]++[(Coordenada x (y+1))]
 |(player == Gote) && (x==9) && (y/=1) && (y/=9) = [(Coordenada x (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x-1) y)]++[(Coordenada x (y+1))]
 |(player == Gote) && (x==1) && (y/=1) && (y/=9) = [(Coordenada x (y-1))]++[(Coordenada (x+1) (y-1))]++[(Coordenada (x+1) y)]++[(Coordenada x (y+1))]
 |(player == Sente) && (x/=1) && (x/=9) && (y/=1) && (y/=9) = [(Coordenada (x+1) y)]++[(Coordenada (x-1) y)]++[(Coordenada (x+1) (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x+1) (y+1))]++[(Coordenada (x-1) (y+1))]++[(Coordenada x (y+1))]
 |(player == Gote) && (x/=1) && (x/=9) && (y/=1) && (y/=9) = [(Coordenada (x+1) y)]++[(Coordenada (x-1) y)]++[(Coordenada (x+1) (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x+1) (y+1))]++[(Coordenada (x-1) (y+1))]++[(Coordenada x (y-1))]
 |otherwise = []

caso1esMovimientoPosibleGeneralDorado = (esMovimientoPosibleGeneralDorado (GeneralDorado, (Coordenada 5 5), Sente))==[(Coordenada 6 5), (Coordenada 4 5), (Coordenada 6 4), (Coordenada 4 4), (Coordenada 6 6), (Coordenada 4 6), (Coordenada 5 6)]
caso2esMovimientoPosibleGeneralDorado = (esMovimientoPosibleGeneralDorado (GeneralDorado, (Coordenada 1 1), Sente)) == [(Coordenada 1 2), (Coordenada 2 2), (Coordenada 2 1)]

casosesMovimientoPosibleGeneralDorado = caso1esMovimientoPosibleGeneralDorado:caso2esMovimientoPosibleGeneralDorado:[]

todoBienesMovimientoPosibleGeneralDorado = and casosesMovimientoPosibleGeneralDorado

esMovimientoPosibleCaballo :: (Pieza, Coordenada, ShogiPlayer) -> [Coordenada]
esMovimientoPosibleCaballo (p, (Coordenada x y), player)
 |(player == Sente) && (y<=7) && (x/=1) && (x/=9) = [(Coordenada (x+1) (y+2))]++[(Coordenada (x-1) (y+2))]
 |(player == Sente) && (y<=7) && (x==1) = [(Coordenada (x+1) (y+2))]
 |(player == Sente) && (y<=7) && (x==9) = [(Coordenada (x-1) (y+2))]
 |(player == Gote) && (y>=3) && (x/=1) && (x/=9) = [(Coordenada (x+1) (y-2))]++[(Coordenada (x-1) (y-2))]
 |(player == Gote) && (y>=3) && (x==1) = [(Coordenada (x+1) (y-2))]
 |(player == Gote) && (y>=3) && (x==9) = [(Coordenada (x-1) (y-2))]
 |otherwise = []

caso1esMovimientoPosibleCaballo = (esMovimientoPosibleCaballo (Caballo, (Coordenada 5 5), Sente))==[(Coordenada 6 7), (Coordenada 4 7)]

casosesMovimientoPosibleCaballo = caso1esMovimientoPosibleCaballo:[]

todoBienesMovimientoPosibleCaballo = and casosesMovimientoPosibleCaballo


esMovimientoPosibleRey :: (Pieza, Coordenada, ShogiPlayer) -> [Coordenada]
esMovimientoPosibleRey (p, (Coordenada x y), player) 
 |(y==9) && (x/=1) && (x/=9) = [(Coordenada x (y-1))]++[(Coordenada (x+1) (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x+1) y)]++[(Coordenada (x-1) y)]
 |(y==9) && (x==1) = [(Coordenada x (y-1))]++[(Coordenada (x+1) (y-1))]++[(Coordenada (x+1) y)]
 |(y==9) && (x==9) = [(Coordenada x (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x-1) y)]
 |(y==1) && (x/=1) && (x/=9) = [(Coordenada (x+1) y)]++[(Coordenada (x+1) (y+1))]++[(Coordenada x (y+1))]++[(Coordenada (x-1) (y+1))]++[(Coordenada (x-1) y)]
 |(y==1) && (x==1) = [(Coordenada (x+1) y)]++[(Coordenada (x+1) (y+1))]++[(Coordenada x (y+1))]
 |(y==1) && (x==9) = [(Coordenada (x-1) y)]++[(Coordenada (x-1) (y+1))]++[(Coordenada x (y+1))]
 |(x==9) && (y/=1) && (y/=9) = [(Coordenada x (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x-1) y)]++[(Coordenada (x-1) (y+1))]++[(Coordenada x (y+1))]
 |(x==1) && (y/=1) && (y/=9) = [(Coordenada x (y-1))]++[(Coordenada (x+1) (y-1))]++[(Coordenada (x+1) y)]++[(Coordenada (x+1) (y+1))]++[(Coordenada x (y+1))]
 |(x/=1) && (x/=9) && (y/=1) && (y/=9) = [(Coordenada x (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x-1) y)]++[(Coordenada (x-1) (y+1))]++[(Coordenada x (y+1))]++[(Coordenada (x+1) (y-1))]++[(Coordenada (x+1) y)]++[(Coordenada (x+1) (y+1))]
 |otherwise = []

caso1esMovimientoPosibleRey = (esMovimientoPosibleRey (Rey, (Coordenada 5 5), Sente))==[(Coordenada 5 4), (Coordenada 4 4), (Coordenada 4 5), (Coordenada 4 6), (Coordenada 5 6), (Coordenada 6 4), (Coordenada 6 5), (Coordenada 6 6)]

casosesMovimientoPosibleRey = caso1esMovimientoPosibleRey:[]

todoBienesMovimientoPosibleRey = and casosesMovimientoPosibleRey

esMovimientoPosibleGeneralPlateado :: (Pieza, Coordenada, ShogiPlayer) -> [Coordenada]
esMovimientoPosibleGeneralPlateado (p, (Coordenada x y), player)
 |(player == Sente) && (y==9) && (x/=1) && (x/=9) = [(Coordenada (x-1) (y-1))]++[(Coordenada (x+1) (y-1))]
 |(player == Sente) && (y==9) && (x==1) = [(Coordenada (x+1) (y-1))]
 |(player == Sente) && (y==9) && (x==9) = [(Coordenada (x-1) (y-1))]
 |(player == Sente) && (y==1) && (x/=1) && (x/=9) = [(Coordenada (x+1) (y+1))]++[(Coordenada x (y+1))]++[(Coordenada (x-1) (y+1))]
 |(player == Sente) && (y==1) && (x==1) = [(Coordenada (x+1) (y+1))]++[(Coordenada x (y+1))]
 |(player == Sente) && (y==1) && (x==9) = [(Coordenada x (y+1))]++[(Coordenada (x-1) (y+1))]
 |(player == Sente) && (x==9) && (y/=1) && (y/=9) = [(Coordenada x (y+1))]++[(Coordenada (x-1) (y+1))]++[(Coordenada (x-1) (y-1))]
 |(player == Sente) && (x==1) && (y/=1) && (y/=9) = [(Coordenada x (y+1))]++[(Coordenada (x+1) (y+1))]++[(Coordenada (x+1) (y-1))]
 |(player == Gote) && (y==9) && (x/=1) && (x/=9) = [(Coordenada x (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x+1) (y-1))]
 |(player == Gote) && (y==9) && (x==1) = [(Coordenada x (y-1))]++[(Coordenada (x+1) (y-1))]
 |(player == Gote) && (y==9) && (x==9) = [(Coordenada x (y-1))]++[(Coordenada (x-1) (y-1))]
 |(player == Gote) && (y==1) && (x/=1) && (x/=9) = [(Coordenada (x+1) (y+1))]++[(Coordenada (x-1) (y+1))]
 |(player == Gote) && (y==1) && (x==1) = [(Coordenada (x+1) (y+1))]
 |(player == Gote) && (y==1) && (x==9) = [(Coordenada (x-1) (y+1))]
 |(player == Gote) && (x==9) && (y/=1) && (y/=9) = [(Coordenada x (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x-1) (y+1))]
 |(player == Gote) && (x==1) && (y/=1) && (y/=9) = [(Coordenada x (y-1))]++[(Coordenada (x+1) (y-1))]++[(Coordenada (x+1) (y+1))]
 |(x/=1) && (x/=9) && (y/=1) && (y/=9) = [(Coordenada (x+1) (y-1))]++[(Coordenada x (y-1))]++[(Coordenada (x-1) (y-1))]++[(Coordenada (x+1) (y+1))]++[(Coordenada (x-1) (y+1))]
 |otherwise = []

caso1esMovimientoPosibleGeneralPlateado = (esMovimientoPosibleGeneralPlateado (GeneralPlateado, (Coordenada 5 5), Sente)) == [(Coordenada 6 4), (Coordenada 5 4), (Coordenada 4 4), (Coordenada 6 6), (Coordenada 4 6)]

casosesMovimientoPosibleGeneralPlateado = caso1esMovimientoPosibleGeneralPlateado:[]

todoBienesMovimientoPosibleGeneralPlateado = and casosesMovimientoPosibleGeneralPlateado

esMovimientoPosibleAlfil :: (Pieza, Coordenada, ShogiPlayer) -> [Coordenada]
esMovimientoPosibleAlfil (Alfil, (a,b), player) = filter (\(p,(x,y)) -> x>0 && x<10 && y>0 && y<10 && (x,y)/=(a,b)) [(Alfil, (a+x, b+x), player | x<-[-8..8]]

esMovimientoPosibleAlfil2 :: (Pieza, Coordenada, ShogiPlayer) -> [Coordenada]
esMovimientoPosibleAlfil2 (Alfil2, (a,b), player) = filter (\(p,(x,y)) -> x>0 && x<10 && y>0 && y<10 && (x,y)/=(a,b)) ([(Alfil2, (a+x,b+x), player) | x<-[-8..8]]++[(Alfil2, (a+1,b), player),(Alfil2, (a,b-1), player),(Alfil2 ,(a,b+1), player),(Alfil2 ,(a-1,b), player)])

esMovimientoPosibleTorre :: (Pieza, Coordenada, ShogiPlayer) -> [Coordenada]
esMovimientoPosibleTorre (Torre, (a,b), player) = filter (\(p,(x,y)) -> x>0 && x<10 && y>0 && y<10 && (x,y)/=(a,b)) ([(Torre ,(a+x,b), player) | x<-[-8..8]]++[(Torre ,(a,b+x), player) | x<-[-8..8]]) 

esMovimientoPosibleTorre2 :: (Pieza, Coordenada, ShogiPlayer) -> [Coordenada]
esMovimientoPosibleTorre2 = (Torre2, (a,b), player) = filter (\(p,(x,y)) -> x>0 && x<10 && y>0 && y<10 && (x,y)/=(a,b)) (([(Torre2 ,(a+x,b), player) | x<-[-8..8]]++[(Torre2 ,(a,b+x), player) | x<-[-8..8]])++[(Torre2 ,(a+1,b+1), player),(Torre2 ,(a-1,b+1), player),(Torre2 ,(a-1,b-1), player),(Torre2 ,(a+1,b-1), player)])
