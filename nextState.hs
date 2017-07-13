import Data.Maybe
import Shogii


activePlayer :: ShogiGame -> Maybe ShogiPlayer
activePlayer (ShogiGame a _) = a 

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

casoArrojar = (nextState (ShogiGame (Just Sente)  [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 5 4), Sente), (Caballo, (Coordenada 0 0), Sente)]) (Sente) (Arrojar Caballo (Coordenada 5 2)))==ShogiGame (Just Gote) [(Rey,Coordenada 3 2,Sente),(Rey,Coordenada 5 4,Sente),(Caballo,Coordenada 5 2,Sente)]

casos = casoArrojar:[]

todoBien = and casos