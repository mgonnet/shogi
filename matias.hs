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

actions:: ShogiGame -> ShogiPlayer -> [ShogiAction]
actions game@(ShogiGame (Just activo) listaFichas) player
  | activo == player = actionsFichasEnTablero ++ (actionsDeLaMano game player)
  | otherwise = []
  where actionsFichasEnTablero = foldl1 (++) [(if duenio==player then (actionsDeUnaPieza ficha game) else []) | ficha@(pieza,coord,duenio) <- listaFichas]

actionsDeLaMano::ShogiGame -> ShogiPlayer -> [ShogiAction]
actionsDeLaMano game@(ShogiGame activo listaFichas) player = foldl1 (++) (map (\ficha@(pieza,coord,duenio) -> if (duenio==player)&&(coord==(Coordenada 0 0)) then (map (\coordLibre -> (Arrojar pieza coordLibre)) (coordsLibresParaPlayer game player)) else []) listaFichas)

coordsLibresParaPlayer::ShogiGame -> ShogiPlayer -> [Coordenada]
coordsLibresParaPlayer game@(ShogiGame activo listaFichas) player = filter (esCoordLibreParaPlayer) todasLasCoordenadas
  where esCoordLibreParaPlayer = (\coord -> ((getCoordenada coord listaFichas)==Nothing))

todoBiencoordsLibresParaPlayer = (coordsLibresParaPlayer beginning Sente)==[Coordenada 1 2,Coordenada 1 4,Coordenada 1 5,Coordenada 1 6,Coordenada 1 8,Coordenada 2 4,Coordenada 2 5,Coordenada 2 6,Coordenada 3 2,Coordenada 3 4,Coordenada 3 5,Coordenada 3 6,Coordenada 3 8,Coordenada 4 2,Coordenada 4 4,Coordenada 4 5,Coordenada 4 6,Coordenada 4 8,Coordenada 5 2,Coordenada 5 4,Coordenada 5 5,Coordenada 5 6,Coordenada 5 8,Coordenada 6 2,Coordenada 6 4,Coordenada 6 5,Coordenada 6 6,Coordenada 6 8,Coordenada 7 2,Coordenada 7 4,Coordenada 7 5,Coordenada 7 6,Coordenada 7 8,Coordenada 8 4,Coordenada 8 5,Coordenada 8 6,Coordenada 9 2,Coordenada 9 4,Coordenada 9 5,Coordenada 9 6,Coordenada 9 8]

caso1actionsDeLaMano = (actionsDeLaMano (ShogiGame (Just Sente) [(Peon,(Coordenada 0 0), Sente)]) Sente)==[Arrojar Peon (Coordenada 1 1),Arrojar Peon (Coordenada 1 2),Arrojar Peon (Coordenada 1 3),Arrojar Peon (Coordenada 1 4),Arrojar Peon (Coordenada 1 5),Arrojar Peon (Coordenada 1 6),Arrojar Peon (Coordenada 2 1),Arrojar Peon (Coordenada 2 2),Arrojar Peon (Coordenada 2 3),Arrojar Peon (Coordenada 2 4),Arrojar Peon (Coordenada 2 5),Arrojar Peon (Coordenada 2 6),Arrojar Peon (Coordenada 3 1),Arrojar Peon (Coordenada 3 2),Arrojar Peon (Coordenada 3 3),Arrojar Peon (Coordenada 3 4),Arrojar Peon (Coordenada 3 5),Arrojar Peon (Coordenada 3 6),Arrojar Peon (Coordenada 4 1),Arrojar Peon (Coordenada 4 2),Arrojar Peon (Coordenada 4 3),Arrojar Peon (Coordenada 4 4),Arrojar Peon (Coordenada 4 5),Arrojar Peon (Coordenada 4 6),Arrojar Peon (Coordenada 5 1),Arrojar Peon (Coordenada 5 2),Arrojar Peon (Coordenada 5 3),Arrojar Peon (Coordenada 5 4),Arrojar Peon (Coordenada 5 5),Arrojar Peon (Coordenada 5 6),Arrojar Peon (Coordenada 6 1),Arrojar Peon (Coordenada 6 2),Arrojar Peon (Coordenada 6 3),Arrojar Peon (Coordenada 6 4),Arrojar Peon (Coordenada 6 5),Arrojar Peon (Coordenada 6 6),Arrojar Peon (Coordenada 7 1),Arrojar Peon (Coordenada 7 2),Arrojar Peon (Coordenada 7 3),Arrojar Peon (Coordenada 7 4),Arrojar Peon (Coordenada 7 5),Arrojar Peon (Coordenada 7 6),Arrojar Peon (Coordenada 8 1),Arrojar Peon (Coordenada 8 2),Arrojar Peon (Coordenada 8 3),Arrojar Peon (Coordenada 8 4),Arrojar Peon (Coordenada 8 5),Arrojar Peon (Coordenada 8 6),Arrojar Peon (Coordenada 9 1),Arrojar Peon (Coordenada 9 2),Arrojar Peon (Coordenada 9 3),Arrojar Peon (Coordenada 9 4),Arrojar Peon (Coordenada 9 5),Arrojar Peon (Coordenada 9 6)]
caso2actionsDeLaMano = (actionsDeLaMano (ShogiGame (Just Sente) [(Peon,(Coordenada 0 0), Sente), (Peon, (Coordenada 1 1), Sente)]) Sente)==[Arrojar Peon (Coordenada 1 2),Arrojar Peon (Coordenada 1 3),Arrojar Peon (Coordenada 1 4),Arrojar Peon (Coordenada 1 5),Arrojar Peon (Coordenada 1 6),Arrojar Peon (Coordenada 2 1),Arrojar Peon (Coordenada 2 2),Arrojar Peon (Coordenada 2 3),Arrojar Peon (Coordenada 2 4),Arrojar Peon (Coordenada 2 5),Arrojar Peon (Coordenada 2 6),Arrojar Peon (Coordenada 3 1),Arrojar Peon (Coordenada 3 2),Arrojar Peon (Coordenada 3 3),Arrojar Peon (Coordenada 3 4),Arrojar Peon (Coordenada 3 5),Arrojar Peon (Coordenada 3 6),Arrojar Peon (Coordenada 4 1),Arrojar Peon (Coordenada 4 2),Arrojar Peon (Coordenada 4 3),Arrojar Peon (Coordenada 4 4),Arrojar Peon (Coordenada 4 5),Arrojar Peon (Coordenada 4 6),Arrojar Peon (Coordenada 5 1),Arrojar Peon (Coordenada 5 2),Arrojar Peon (Coordenada 5 3),Arrojar Peon (Coordenada 5 4),Arrojar Peon (Coordenada 5 5),Arrojar Peon (Coordenada 5 6),Arrojar Peon (Coordenada 6 1),Arrojar Peon (Coordenada 6 2),Arrojar Peon (Coordenada 6 3),Arrojar Peon (Coordenada 6 4),Arrojar Peon (Coordenada 6 5),Arrojar Peon (Coordenada 6 6),Arrojar Peon (Coordenada 7 1),Arrojar Peon (Coordenada 7 2),Arrojar Peon (Coordenada 7 3),Arrojar Peon (Coordenada 7 4),Arrojar Peon (Coordenada 7 5),Arrojar Peon (Coordenada 7 6),Arrojar Peon (Coordenada 8 1),Arrojar Peon (Coordenada 8 2),Arrojar Peon (Coordenada 8 3),Arrojar Peon (Coordenada 8 4),Arrojar Peon (Coordenada 8 5),Arrojar Peon (Coordenada 8 6),Arrojar Peon (Coordenada 9 1),Arrojar Peon (Coordenada 9 2),Arrojar Peon (Coordenada 9 3),Arrojar Peon (Coordenada 9 4),Arrojar Peon (Coordenada 9 5),Arrojar Peon (Coordenada 9 6)]
caso3actionsDeLaMano = (actionsDeLaMano (ShogiGame (Just Sente) [(Peon,(Coordenada 0 0), Sente), (Peon, (Coordenada 1 1), Gote)]) Sente)==[Arrojar Peon (Coordenada 1 2),Arrojar Peon (Coordenada 1 3),Arrojar Peon (Coordenada 1 4),Arrojar Peon (Coordenada 1 5),Arrojar Peon (Coordenada 1 6),Arrojar Peon (Coordenada 2 1),Arrojar Peon (Coordenada 2 2),Arrojar Peon (Coordenada 2 3),Arrojar Peon (Coordenada 2 4),Arrojar Peon (Coordenada 2 5),Arrojar Peon (Coordenada 2 6),Arrojar Peon (Coordenada 3 1),Arrojar Peon (Coordenada 3 2),Arrojar Peon (Coordenada 3 3),Arrojar Peon (Coordenada 3 4),Arrojar Peon (Coordenada 3 5),Arrojar Peon (Coordenada 3 6),Arrojar Peon (Coordenada 4 1),Arrojar Peon (Coordenada 4 2),Arrojar Peon (Coordenada 4 3),Arrojar Peon (Coordenada 4 4),Arrojar Peon (Coordenada 4 5),Arrojar Peon (Coordenada 4 6),Arrojar Peon (Coordenada 5 1),Arrojar Peon (Coordenada 5 2),Arrojar Peon (Coordenada 5 3),Arrojar Peon (Coordenada 5 4),Arrojar Peon (Coordenada 5 5),Arrojar Peon (Coordenada 5 6),Arrojar Peon (Coordenada 6 1),Arrojar Peon (Coordenada 6 2),Arrojar Peon (Coordenada 6 3),Arrojar Peon (Coordenada 6 4),Arrojar Peon (Coordenada 6 5),Arrojar Peon (Coordenada 6 6),Arrojar Peon (Coordenada 7 1),Arrojar Peon (Coordenada 7 2),Arrojar Peon (Coordenada 7 3),Arrojar Peon (Coordenada 7 4),Arrojar Peon (Coordenada 7 5),Arrojar Peon (Coordenada 7 6),Arrojar Peon (Coordenada 8 1),Arrojar Peon (Coordenada 8 2),Arrojar Peon (Coordenada 8 3),Arrojar Peon (Coordenada 8 4),Arrojar Peon (Coordenada 8 5),Arrojar Peon (Coordenada 8 6),Arrojar Peon (Coordenada 9 1),Arrojar Peon (Coordenada 9 2),Arrojar Peon (Coordenada 9 3),Arrojar Peon (Coordenada 9 4),Arrojar Peon (Coordenada 9 5),Arrojar Peon (Coordenada 9 6)]

casosactionsDeLaMano = caso1actionsDeLaMano:caso2actionsDeLaMano:caso3actionsDeLaMano:[]
todoBienactionsDeLaMano = and casosactionsDeLaMano

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
caso8actionsDeUnaPieza = (actionsDeUnaPieza (Torre, (Coordenada 1 1), Sente) (ShogiGame (Just Sente) [(Torre, (Coordenada 1 1), Sente),(Peon, (Coordenada 1 3), Sente),(Peon, (Coordenada 3 1), Sente)])) == [(Movimiento (Coordenada 1 1) (Coordenada 1 2) False),(Movimiento (Coordenada 1 1) (Coordenada 2 1) False)]
caso9ActionsDeUnaPieza = (actionsDeUnaPieza (Torre, (Coordenada 8 2), Sente) beginning)==[Movimiento (Coordenada 8 2) (Coordenada 9 2) False,Movimiento (Coordenada 8 2) (Coordenada 7 2) False,Movimiento (Coordenada 8 2) (Coordenada 6 2) False,Movimiento (Coordenada 8 2) (Coordenada 5 2) False,Movimiento (Coordenada 8 2) (Coordenada 4 2) False,Movimiento (Coordenada 8 2) (Coordenada 3 2) False]
caso10ActionsDeUnaPieza = (actionsDeUnaPieza (GeneralDorado, (Coordenada 4 1), Sente) beginning)==[Movimiento (Coordenada 4 1) (Coordenada 4 2) False,Movimiento (Coordenada 4 1) (Coordenada 5 2) False,Movimiento (Coordenada 4 1) (Coordenada 3 2) False]
caso11ActionsDeUnaPieza = (actionsDeUnaPieza (Caballo, (Coordenada 2 1), Sente) beginning)==[]
caso12ActionsDeUnaPieza = (actionsDeUnaPieza (Caballo, (Coordenada 1 1), Sente) (ShogiGame (Just Sente) [(Caballo, (Coordenada 1 1), Sente)]))==[(Movimiento (Coordenada 1 1) (Coordenada 2 3) False)]
caso13ActionsDeUnaPieza = (actionsDeUnaPieza (Caballo, (Coordenada 3 1), Sente) (ShogiGame (Just Sente) [(Caballo, (Coordenada 3 1), Sente)]))==[(Movimiento (Coordenada 3 1) (Coordenada 4 3) False), (Movimiento (Coordenada 3 1) (Coordenada 2 3) False)]
caso14ActionsDeUnaPieza = (actionsDeUnaPieza (Rey, (Coordenada 5 1), Sente) beginning)==[Movimiento (Coordenada 5 1) (Coordenada 6 2) False,Movimiento (Coordenada 5 1) (Coordenada 5 2) False,Movimiento (Coordenada 5 1) (Coordenada 4 2) False]
caso15ActionsDeUnaPieza = (actionsDeUnaPieza (Rey, (Coordenada 3 3), Sente) (ShogiGame (Just Sente) [(Caballo, (Coordenada 3 3), Sente)]))==[Movimiento (Coordenada 3 3) (Coordenada 3 2) False,Movimiento (Coordenada 3 3) (Coordenada 2 2) False,Movimiento (Coordenada 3 3) (Coordenada 2 3) False,Movimiento (Coordenada 3 3) (Coordenada 2 4) False,Movimiento (Coordenada 3 3) (Coordenada 3 4) False,Movimiento (Coordenada 3 3) (Coordenada 4 2) False,Movimiento (Coordenada 3 3) (Coordenada 4 3) False,Movimiento (Coordenada 3 3) (Coordenada 4 4) False]

casosactionsDeUnaPieza = caso1actionsDeUnaPieza:caso2actionsDeUnaPieza:caso3actionsDeUnaPieza:caso4actionsDeUnaPieza:caso5actionsDeUnaPieza:caso6actionsDeUnaPieza:caso7actionsDeUnaPieza:caso8actionsDeUnaPieza:caso9ActionsDeUnaPieza:caso10ActionsDeUnaPieza:caso11ActionsDeUnaPieza:caso12ActionsDeUnaPieza:caso13ActionsDeUnaPieza:caso14ActionsDeUnaPieza:caso15ActionsDeUnaPieza:[]

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
obtenerFuncionMovimientoDePieza Torre = movimientosPosiblesTorre
obtenerFuncionMovimientoDePieza GeneralDorado = esMovimientoPosibleGeneralDorado
obtenerFuncionMovimientoDePieza Caballo = esMovimientoPosibleCaballo
obtenerFuncionMovimientoDePieza Rey = esMovimientoPosibleRey
obtenerFuncionMovimientoDePieza GeneralPlatada = esMovimientoPosibleGeneralPlateado



puedePromover :: ShogiAction -> Pieza -> ShogiPlayer -> Bool
puedePromover _ Rey _ = False
puedePromover _ GeneralDorado _ = False
puedePromover (Movimiento coord1 coord2 _ ) _ player = not (enCampoPropio coord2 player)

enCampoPropio:: Coordenada -> ShogiPlayer -> Bool
enCampoPropio (Coordenada w z) Sente = z<7
enCampoPropio (Coordenada w z) Gote = z>3

puedePromoverCaso1 = (puedePromover (Movimiento (Coordenada 1 1) (Coordenada 2 8) True) Peon Sente)==True

casospuedePromover = puedePromoverCaso1:[]

todoBienpuedePromover = and casospuedePromover



-----