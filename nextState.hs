module NextState where
import Data.Maybe
import Data.List.Split
import Shogii

isFinished :: ShogiGame -> Bool
isFinished (ShogiGame _ a) = (length (filter (\(x,y,z) -> ((x==Rey)&&(y/=(Coordenada 0 0))) ) a))==1

caso1 = (isFinished (ShogiGame (Just Sente) [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 3 3), Gote)]))==False
--Tengo dos reyes
caso2 = (isFinished (ShogiGame (Just Sente) [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 3 3), Gote), (Caballo, (Coordenada 3 3), Gote)]))==False
--Tengo dos reyes y un caballo
caso3 = (isFinished (ShogiGame (Just Sente) [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 0 0), Sente), (Caballo, (Coordenada 3 3), Gote)]))==True
--Tengo un rey y un caballo
caso4 = (isFinished (ShogiGame (Just Sente) [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 0 0), Sente)]))==True
--Tengo solo un rey

casosIsFinished = caso1:caso2:caso3:caso4:[]

todoBienIsFinishe = and casosIsFinished


activePlayer :: ShogiGame -> Maybe ShogiPlayer
activePlayer (ShogiGame a _) = a 

----------- Lo nuevo de nextState----------


nextState:: ShogiGame -> ShogiPlayer -> ShogiAction -> ShogiGame
nextState x@(ShogiGame (Just a) _) y z =  if (a/=y) then error "El jugador no esta activo" else (if (isFinished x) then error "el juego esta terminado" else (nextStateValido x y z))




nextStateValido:: ShogiGame -> ShogiPlayer -> ShogiAction -> ShogiGame
--nextStateValido a b (Movimiento cord1 cord2 promover) = caso de movimiento 
nextStateValido (ShogiGame a listaDePiezas) juegadorQueMueve (Arrojar pieza coord1) = (ShogiGame (cambioJugadorActivo a) (map (\(x,y,z) -> if (((x==pieza) && (y==(Coordenada 0 0))) && (z==juegadorQueMueve)) then (x,coord1,juegadorQueMueve) else (x,y,z)) listaDePiezas) )
nextStateValido (ShogiGame a listaDePiezas) juegadorQueMueve (Movimiento coord1 coord2 promover) = (ShogiGame (cambioJugadorActivo a) (map (\(x,y,z) -> if (y==coord1) then ((promoverPieza x promover), coord2, z) else (if (y==coord2) then ((x,(Coordenada 0 0), (cambiarJugador z))) else (x,y,z) )) listaDePiezas))

promoverPieza:: Pieza -> Bool -> Pieza
promoverPieza Alfil True = Alfil2
promoverPieza Torre True = Torre2
promoverPieza GeneralPlateado True = GeneralPlateado2
promoverPieza GeneralDorado _ = GeneralDorado
promoverPieza Peon True = Peon2
promoverPieza Lancero True = Lancero2
promoverPieza Caballo True = Caballo2
promoverPieza a _ = a

cambioJugadorActivo:: Maybe ShogiPlayer-> Maybe ShogiPlayer
cambioJugadorActivo (Just Sente) = (Just Gote) 
cambioJugadorActivo (Just Gote) = (Just Sente)
cambioJugadorActivo Nothing = Nothing



------- Test Cases -------
casoJugadorNoValido = (nextState (ShogiGame (Just Sente) []) (Gote) (Movimiento (Coordenada 2 3) (Coordenada 4 5) True))
casoJuegoTeminado = (nextState (ShogiGame (Just Sente) [(Rey, (Coordenada 3 2), Sente)]) (Sente) (Movimiento (Coordenada 2 3) (Coordenada 4 5) True))

casoArrojar = (nextState (ShogiGame (Just Sente)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Sente), (Caballo, (Coordenada 0 0), Sente)]) (Sente) (Arrojar Caballo (Coordenada 5 2)))==ShogiGame (Just Gote) [(Rey,Coordenada 3 2,Sente),(Rey,Coordenada 5 4,Sente),(Caballo,Coordenada 5 2,Sente)]

casoMoverSinComer = (nextState (ShogiGame (Just Sente)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Gote), (Caballo, (Coordenada 1 1), Sente)]) (Sente) (Movimiento (Coordenada 1 1) (Coordenada 1 3) False))==(ShogiGame (Just Gote)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Gote), (Caballo, (Coordenada 1 3), Sente)])
casoMoverSinComerYPromuevo = (nextState (ShogiGame (Just Sente)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Gote), (Caballo, (Coordenada 1 1), Sente)]) (Sente) (Movimiento (Coordenada 1 1) (Coordenada 1 3) True))==(ShogiGame (Just Gote)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Gote), (Caballo2, (Coordenada 1 3), Sente)])

casoMoverYComer = (nextState (ShogiGame (Just Sente)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Gote), (Caballo, (Coordenada 1 1), Sente), (Caballo, (Coordenada 1 3), Gote) ]) (Sente) (Movimiento (Coordenada 1 1) (Coordenada 1 3) False))==(ShogiGame (Just Gote)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Gote), (Caballo, (Coordenada 1 3), Sente), (Caballo, (Coordenada 0 0), Sente)])
casoMoverYComerYPromover = (nextState (ShogiGame (Just Sente)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Gote), (Caballo, (Coordenada 1 1), Sente), (Caballo, (Coordenada 1 3), Gote) ]) (Sente) (Movimiento (Coordenada 1 1) (Coordenada 1 3) True))==(ShogiGame (Just Gote)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Gote), (Caballo2, (Coordenada 1 3), Sente), (Caballo, (Coordenada 0 0), Sente)])


casosNextState = casoArrojar:casoMoverSinComer:casoMoverYComer:casoMoverYComerYPromover:casoMoverSinComerYPromuevo:[]

todoBien = and (casosNextState++casosIsFinished)



------------------

readAction :: String -> ShogiAction
readAction x
  |length(x) == 22 = (Movimiento (auxStringCoord (take 3 (drop 6 x))) (auxStringCoord (take 3 (drop 6 x))) True)
  |length(x) == 24 = (Movimiento (auxStringCoord (take 3 (drop 10 x))) (auxStringCoord (take 3 (drop 10 x))) False)
  |length(x) == 13 = (Arrojar (auxStringPieza((drop 8 x)!!0)) (auxStringCoord(take 3(drop 10 x))))
  |otherwise = error "Accion incorrecta"

auxStringCoord :: String -> Coordenada
auxStringCoord x
  |((length x) == 3)&&((x!!1)==',') =  (Coordenada (read ((splitOn "," x)!!0)::Int) (read ((splitOn "," x)!!1)::Int))
  |otherwise = error "String no valido"

auxStringPieza :: Char -> Pieza
auxStringPieza x
  |x == 'p' = Peon
  |x == 'r' = Rey
  |x == 'g' = GeneralDorado
  |x == 's' = GeneralPlateado
  |x == 't' = Torre
  |x == 'a' = Alfil
  |x == 'l' = Lancero
  |x == 'c' = Caballo
  |otherwise = error "Caracter no valido"

score :: ShogiGame -> ShogiPlayer -> Maybe Int
score (ShogiGame (Just a) b) p
  |((isFinished(ShogiGame (Just a) b)) && (a == p)) = Just 1
  |((isFinished(ShogiGame (Just a) b)) && (a /= p)) = Just (-1)
  |otherwise = Nothing
score _ _ = Nothing