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


--showBoard :: ShogiGame -> String





caso1 = (showAction (Arrojar Rey (Coordenada 3 2))=="Arrojar R 3,2")
caso2 = (showAction (Movimiento (Coordenada 3 2) (Coordenada 3 3) True))=="Mover 3,2 3,3 True"

casos = caso1:caso2:[]

todoBien = and casos
