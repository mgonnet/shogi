import Shogii

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
  |((isFinished(ShogiGame (Just a) b)) && (a == p)) = 1
  |((isFinished(ShogiGame (Just a) b)) && (a /= p)) = -1
  |otherwise = Nothing