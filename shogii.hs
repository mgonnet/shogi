--Sente ARRIBA
--Gote ABAJO

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