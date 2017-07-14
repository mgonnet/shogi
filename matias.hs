import Shogii
import MovimientosPiezas
import NextState

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

caso1showAction = (showAction (Arrojar Rey (Coordenada 3 2))=="Arrojar R 3,2")
caso2showAction = (showAction (Movimiento (Coordenada 3 2) (Coordenada 3 3) True))=="Mover 3,2 3,3 True"

casosshowAction = caso1showAction:caso2showAction:[]

todoBienshowAction = and casosshowAction

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

actionsDeUnaPieza :: (Pieza,Coordenada,ShogiPlayer) -> ShogiGame -> [ShogiAction]
actionsDeUnaPieza tripleta@(pieza,coordenada,player) game@(ShogiGame jugador listaFichas) = shogiActionsSinPromover ++ shogiActionsPromovidas
  where shogiActionsPromovidas = (map (\(Movimiento posA posB promover) -> (Movimiento posA posB True)) shogiActionsPromovibles)
        shogiActionsPromovibles = (filter (\action -> (puedePromover action pieza player)) shogiActionsSinPromover)
        shogiActionsSinPromover = map (\coord -> (Movimiento coordenada coord False)) coordenadasFiltradas
        coordenadasFiltradas = foldl1 (++) coordenasPorDireccionFiltradas
        coordenasPorDireccionFiltradas = map (\coords -> (filtrarRecorridoEnUnaDireccion game coords)) ((obtenerFuncionMovimientoDePieza pieza) (tripleta))

caso1actionsDeUnaPieza = (actionsDeUnaPieza (Peon, (Coordenada 1 1), Sente) (ShogiGame (Just Sente) [(Peon, (Coordenada 1 1), Sente)])) == [(Movimiento (Coordenada 1 1) (Coordenada 1 2) False)]
caso2actionsDeUnaPieza = (actionsDeUnaPieza (Peon, (Coordenada 1 1), Sente) (ShogiGame (Just Sente) [(Peon, (Coordenada 1 1), Sente),(Peon, (Coordenada 1 3), Gote)]))== [(Movimiento (Coordenada 1 1) (Coordenada 1 2) False)]
caso3actionsDeUnaPieza = (actionsDeUnaPieza (Peon, (Coordenada 1 1), Sente) (ShogiGame (Just Sente) [(Peon, (Coordenada 1 1), Sente),(Peon, (Coordenada 1 2), Sente)]))== []
caso4actionsDeUnaPieza = (actionsDeUnaPieza (Peon, (Coordenada 1 1), Sente) (ShogiGame (Just Sente) [(Peon, (Coordenada 1 1), Sente),(Peon, (Coordenada 1 2), Gote)]))== [(Movimiento (Coordenada 1 1) (Coordenada 1 2) False)]
caso5actionsDeUnaPieza = (actionsDeUnaPieza (Peon, (Coordenada 1 7), Sente) (ShogiGame (Just Sente) [(Peon, (Coordenada 1 7), Sente)])) == [(Movimiento (Coordenada 1 7) (Coordenada 1 8) False), (Movimiento (Coordenada 1 7) (Coordenada 1 8) True)]
caso6actionsDeUnaPieza = (actionsDeUnaPieza (Lancero, (Coordenada 1 1), Sente) (ShogiGame (Just Sente) [(Lancero, (Coordenada 1 1), Sente),(Peon, (Coordenada 1 4), Sente)])) == [(Movimiento (Coordenada 1 1) (Coordenada 1 2) False), (Movimiento (Coordenada 1 1) (Coordenada 1 3) False)]
caso7actionsDeUnaPieza = (actionsDeUnaPieza (Lancero, (Coordenada 1 5), Sente) (ShogiGame (Just Sente) [(Lancero, (Coordenada 1 5), Sente),(Peon, (Coordenada 1 8), Sente)])) == [(Movimiento (Coordenada 1 5) (Coordenada 1 6) False), (Movimiento (Coordenada 1 5) (Coordenada 1 7) False), (Movimiento (Coordenada 1 5) (Coordenada 1 7) True)]

casosactionsDeUnaPieza = caso1actionsDeUnaPieza:caso2actionsDeUnaPieza:caso3actionsDeUnaPieza:caso4actionsDeUnaPieza:caso5actionsDeUnaPieza:caso6actionsDeUnaPieza:caso7actionsDeUnaPieza:[]

todoBienactionsDeUnaPieza = and casosactionsDeUnaPieza

filtrarRecorridoEnUnaDireccion:: ShogiGame -> [Coordenada] -> [Coordenada]
filtrarRecorridoEnUnaDireccion _ [] = []
filtrarRecorridoEnUnaDireccion game@(ShogiGame (Just a) listaFichas) (coord1:xs) = if (length fichasOcupandoPosicion)==0 then (coord1:(filtrarRecorridoEnUnaDireccion game xs)) else (if (((\(t1,t2,t3) -> t3) (head fichasOcupandoPosicion))/=a) then coord1:[] else [])
  where fichasOcupandoPosicion = (filter (\(x,y,z) -> (y==coord1) ) listaFichas)

caso1filtrarRecorridoEnUnaDireccion = (filtrarRecorridoEnUnaDireccion (ShogiGame (Just Sente) [(Peon, (Coordenada 1 1), Sente)]) [(Coordenada 1 2)])==[(Coordenada 1 2)]
caso2filtrarRecorridoEnUnaDireccion = (filtrarRecorridoEnUnaDireccion (ShogiGame (Just Sente) [(Peon, (Coordenada 1 1), Sente)]) [(Coordenada 1 2),(Coordenada 1 3),(Coordenada 1 4)])==[(Coordenada 1 2),(Coordenada 1 3),(Coordenada 1 4)]
caso3filtrarRecorridoEnUnaDireccion = (filtrarRecorridoEnUnaDireccion (ShogiGame (Just Sente) [(Peon, (Coordenada 1 1), Sente),(Peon, (Coordenada 1 2), Sente)]) [(Coordenada 1 2)])==[]
caso4filtrarRecorridoEnUnaDireccion = (filtrarRecorridoEnUnaDireccion (ShogiGame (Just Sente) [(Peon, (Coordenada 1 1), Sente),(Peon, (Coordenada 1 2), Gote)]) [(Coordenada 1 2)])==[(Coordenada 1 2)]
caso5filtrarRecorridoEnUnaDireccion = (filtrarRecorridoEnUnaDireccion (ShogiGame (Just Sente) [(Peon, (Coordenada 1 1), Sente),(Peon, (Coordenada 1 3), Sente)]) [(Coordenada 1 2),(Coordenada 1 3),(Coordenada 1 4)])==[(Coordenada 1 2)]
caso6filtrarRecorridoEnUnaDireccion = (filtrarRecorridoEnUnaDireccion (ShogiGame (Just Sente) [(Peon, (Coordenada 1 1), Sente),(Peon, (Coordenada 1 3), Gote)]) [(Coordenada 1 2),(Coordenada 1 3),(Coordenada 1 4)])==[(Coordenada 1 2),(Coordenada 1 3)]

casosfiltrarRecorridoEnUnaDireccion = caso1filtrarRecorridoEnUnaDireccion:caso2filtrarRecorridoEnUnaDireccion:caso3filtrarRecorridoEnUnaDireccion:caso4filtrarRecorridoEnUnaDireccion:caso5filtrarRecorridoEnUnaDireccion:caso6filtrarRecorridoEnUnaDireccion:[]

todoBienfiltrarRecorridoEnUnaDireccion = and casosfiltrarRecorridoEnUnaDireccion

obtenerFuncionMovimientoDePieza :: Pieza -> ((Pieza,Coordenada,ShogiPlayer) -> [[Coordenada]])
obtenerFuncionMovimientoDePieza Peon = movimientosPosiblesPeon
obtenerFuncionMovimientoDePieza Lancero = movimientosPosiblesLancero



puedePromover :: ShogiAction -> Pieza -> ShogiPlayer -> Bool
puedePromover _ Rey _ = False
puedePromover _ GeneralDorado _ = False
puedePromover (Movimiento (Coordenada x y) (Coordenada w z) _) _ Sente = z>=7
puedePromover (Movimiento (Coordenada x y) (Coordenada w z) _) _ Gote = z<=3

puedePromoverCaso1 = (puedePromover (Movimiento (Coordenada 1 1) (Coordenada 2 8) True) Peon Sente)==True

casospuedePromover = puedePromoverCaso1:[]

todoBienpuedePromover = and casospuedePromover


-----