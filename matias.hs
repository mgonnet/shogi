import Shogii

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

caso1 = (showAction (Arrojar Rey (Coordenada 3 2))=="Arrojar R 3,2")
caso2 = (showAction (Movimiento (Coordenada 3 2) (Coordenada 3 3) True))=="Mover 3,2 3,3 True"

casos = caso1:caso2:[]

todoBien = and casos

-----------------------------

showBoard :: ShogiGame -> String
showBoard (ShogiGame a listaFichas) = (foldl1 (\a b -> a++b) [(if x==0 then "\n" else (showPiezaPosicionada (getCoordenada (Coordenada x y) listaFichas))) | y <- [1..9], x <- [9,8,7,6,5,4,3,2,1,0] ]) ++ "\n" ++ "Jugador Activo: " ++ (showMaybePlayer a) ++ "\n"

showMaybePlayer :: Maybe ShogiPlayer -> String
showMaybePlayer Nothing = "Ninguno"
showMaybePlayer (Just a) = (showPlayer a)

showPlayer :: ShogiPlayer -> String
showPlayer Sente = "S"
showPlayer Gote = "G"

showPiezaPosicionada :: Maybe (Pieza, Coordenada, ShogiPlayer) -> String
showPiezaPosicionada Nothing = " *##* "
showPiezaPosicionada (Just (a,b,c)) = " *"++(showPieza a)++(showPlayer c)++"* "

showPiezaPosicionadaCaso1 = (showPiezaPosicionada (Just (Peon, (Coordenada 1 1), Sente)))==" *pS* "

casosShowPiezaPosicionada = showPiezaPosicionadaCaso1:[]

todoBienShowPiezaPosicionada = and casosShowPiezaPosicionada

probarShowBoard1 = showBoard (ShogiGame (Just Sente) [(Peon, (Coordenada 2 3), Sente), (Peon, (Coordenada 0 0), Sente)])