import Data.Maybe

data ShogiPlayer = Sente | Gote deriving (Eq, Show, Enum) 

data Coordenada = Coordenada Int Int deriving (Eq, Show) 

activePlayer :: ShogiGame -> Maybe ShogiPlayer
activePlayer (ShogiGame a _) = a 

data Pieza = Rey | Alfil | Torre | GeneralPlateado | GeneralDorado | Peon | Lancero | Caballo | Alfil2 | Torre2 | GeneralPlateado2 | Peon2 | Lancero2 | Caballo2 deriving (Eq, Enum, Show) 

data ShogiAction = Movimiento Coordenada Coordenada Bool | Arrojar Pieza Coordenada deriving (Eq, Show)
--el bool es por si quiero promover 

data ShogiGame = ShogiGame (Maybe ShogiPlayer) [(Pieza, Coordenada, ShogiPlayer)] deriving (Eq, Show) 

beginning :: ShogiGame
beginning = ShogiGame (Just Sente) (peones++lanceros++caballos++generalesPlateados++generalesDorados++reyes++alfiles++torres)

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

showPieza :: Pieza -> String
showPieza Rey = "R"
showPieza Alfil = "a"
showPieza Torre = "t"
showPieza GeneralPlateado = "s"
showPieza GeneralDorado = "G"
showPieza Peon = "p"
showPieza Lancero = "l"
showPieza Caballo = "c"
showPieza Alfil2 = "A"
showPieza Torre2 = "T"
showPieza GeneralPlateado2 = "S"
showPieza Peon2 = "P"
showPieza Lancero2 = "L"
showPieza Caballo2 = "C"

showAction :: ShogiAction -> String
showAction (Movimiento (Coordenada x1 x2) (Coordenada y1 y2) promover) = "Mover "++(show x1)++","++(show x2)++" "++(show y1)++","++(show y2)++" "++(show promover)
showAction (Arrojar pieza (Coordenada x1 x2)) = "Arrojar "++(showPieza pieza)++" "++(show x1)++","++(show x2)

isFinished :: ShogiGame -> Bool
isFinished (ShogiGame _ a) = (length (filter (\(x,y,z) -> x==Rey) a))==1


----------- Lo nuevo de nextState----------


nextState:: ShogiGame -> ShogiPlayer -> ShogiAction -> ShogiGame
nextState (ShogiGame Nothing _) _ _ = error "el juego esta terminado"
--nextState a b c = if (isFinished a) then error "El juego esta terminado" else (nextStateValido a b c)
nextState x@(ShogiGame (Just a) _) y z =  if (a/=y) then error "El jugador no esta activo" else (nextStateValido x y z)


nextStateValido:: ShogiGame -> ShogiPlayer -> ShogiAction -> ShogiGame
--nextStateValido a b (Movimiento cord1 cord2 promover) = caso de movimiento 
--nextStateValido (ShogiGame a listaDePiezas) b (Arrojar pieza coord1) = (ShogiGame (\x -> if (x==Sente) then Gote else Sente) (map (\(x,y,z) -> if (((x==pieza) && (y==(Coordenada 0 0))) && (z==b)) then (x,coord1,b) else (x,y,z)) listaDePiezas) )
nextStateValido (ShogiGame a listaDePiezas) b (Arrojar pieza coord1) = (ShogiGame (cambioJugadorActivo a) (map (\(x,y,z) -> if (((x==pieza) && (y==(Coordenada 0 0))) && (z==b)) then (x,coord1,b) else (x,y,z)) listaDePiezas) )



cambioJugadorActivo:: Maybe ShogiPlayer-> Maybe ShogiPlayer
cambioJugadorActivo (Just Sente) = (Just Gote) 
cambioJugadorActivo (Just Gote) = (Just Sente)
cambioJugadorActivo Nothing = Nothing



------- Test Cases -------
casoJugadorNoValido = (nextState (ShogiGame (Just Sente) []) (Gote) (Movimiento (Coordenada 2 3) (Coordenada 4 5) True))
casoJuegoTeminado = (nextState (ShogiGame Nothing [(Rey, (Coordenada 3 2), Sente)]) (Sente) (Movimiento (Coordenada 2 3) (Coordenada 4 5) True))

casoArrojar = (nextState (ShogiGame (Just Sente)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Sente), (Caballo, (Coordenada 0 0), Sente)]) (Sente) (Arrojar Caballo (Coordenada 5 2)))