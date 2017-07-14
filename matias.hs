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


-------------------ACTIONS

movimientosPosiblesPeon :: (Pieza, Coordenada, ShogiPlayer) -> [[Coordenada]]
movimientosPosiblesPeon (a, (Coordenada x y), Sente) = if y==9 then [[]] else [[(Coordenada x (y+1))]]
movimientosPosiblesPeon (a, (Coordenada x y), Gote) = if y==1 then [[]] else [[(Coordenada x (y-1))]]

caso1MovimientosPosiblesPeon = (movimientosPosiblesPeon (Peon, (Coordenada 1 1), Sente)) == [[(Coordenada 1 2)]]
caso2MovimientosPosiblesPeon = (movimientosPosiblesPeon (Peon, (Coordenada 1 9), Gote)) == [[(Coordenada 1 8)]]
caso3MovimientosPosiblesPeon = (movimientosPosiblesPeon (Peon, (Coordenada 1 9), Sente)) == [[]]
caso4MovimientosPosiblesPeon = (movimientosPosiblesPeon (Peon, (Coordenada 1 1), Gote)) == [[]]

casosMovimientosPosiblesPeon = caso1MovimientosPosiblesPeon:caso2MovimientosPosiblesPeon:caso3MovimientosPosiblesPeon:caso4MovimientosPosiblesPeon:[]

todoBienMovimientosPosiblesPeon = and casosMovimientosPosiblesPeon


puedePromover :: ShogiAction -> Pieza -> ShogiPlayer -> Bool
puedePromover _ Rey _ = False
puedePromover _ GeneralDorado _ = False
puedePromover (Movimiento (Coordenada x y) (Coordenada w z) a) _ Sente = z>=7
puedePromover (Movimiento (Coordenada x y) (Coordenada w z) a) _ Gote = z<=3

puedePromoverCaso1 = (puedePromover (Movimiento (Coordenada 1 1) (Coordenada 2 8) True) Peon Sente)==True

casospuedePromover = puedePromoverCaso1:[]

todoBienpuedePromover = and casospuedePromover


-----