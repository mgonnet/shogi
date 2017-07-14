module MovimientosPiezas where
import Shogii

movimientosPosiblesPeon :: (Pieza, Coordenada, ShogiPlayer) -> [[Coordenada]]
movimientosPosiblesPeon (a, (Coordenada x y), Sente) = if y==9 then [[]] else [[(Coordenada x (y+1))]]
movimientosPosiblesPeon (a, (Coordenada x y), Gote) = if y==1 then [[]] else [[(Coordenada x (y-1))]]

caso1MovimientosPosiblesPeon = (movimientosPosiblesPeon (Peon, (Coordenada 1 1), Sente)) == [[(Coordenada 1 2)]]
caso2MovimientosPosiblesPeon = (movimientosPosiblesPeon (Peon, (Coordenada 1 9), Gote)) == [[(Coordenada 1 8)]]
caso3MovimientosPosiblesPeon = (movimientosPosiblesPeon (Peon, (Coordenada 1 9), Sente)) == [[]]
caso4MovimientosPosiblesPeon = (movimientosPosiblesPeon (Peon, (Coordenada 1 1), Gote)) == [[]]

casosMovimientosPosiblesPeon = caso1MovimientosPosiblesPeon:caso2MovimientosPosiblesPeon:caso3MovimientosPosiblesPeon:caso4MovimientosPosiblesPeon:[]

todoBienMovimientosPosiblesPeon = and casosMovimientosPosiblesPeon

movimientosPosiblesLancero :: (Pieza, Coordenada, ShogiPlayer) -> [[Coordenada]]
movimientosPosiblesLancero (pieza, (Coordenada columna fila), Sente) = if (fila==9) then [[]] else [[(Coordenada columna fila)| fila <-[(fila+1)..9]]]
movimientosPosiblesLancero (pieza, (Coordenada columna fila), Gote) = if (fila==1) then [[]] else [[(Coordenada columna fila)| fila <- ([(fila-1),(fila-2)..1])]]

movimientosPosiblesTorre::(Pieza, Coordenada, ShogiPlayer) -> [[Coordenada]]
movimientosPosiblesTorre (pieza, (Coordenada columna fila), _) = [[(Coordenada columna fila)| fila <- [(fila+1)..9]]]
                                                                 ++[[(Coordenada columna fila)| fila <- ([(fila-1),(fila-2)..1])]]
                                                                 ++[[(Coordenada columna fila)| columna <- [(columna+1)..9]]]
                                                                 ++[[(Coordenada columna fila)| columna <- ([(columna-1),(columna-2)..1])]]                        

esMovimientoPosibleGeneralDorado :: (Pieza, Coordenada, ShogiPlayer) -> [[Coordenada]]
esMovimientoPosibleGeneralDorado (p, (Coordenada x y), player) 
 |(player == Sente) && (y==9) && (x/=1) && (x/=9) = [[(Coordenada (x+1) y)],[(Coordenada (x-1) y)],[(Coordenada x (y-1))]]
 |(player == Sente) && (y==9) && (x==1) =[[(Coordenada (x+1) y)],[(Coordenada x (y-1))]]
 |(player == Sente) && (y==9) && (x==9) =[[(Coordenada (x-1) y)],[(Coordenada x (y-1))]]
 |(player == Sente) && (y==1) && (x/=1) && (x/=9) =[[(Coordenada x (y+1))],[(Coordenada (x+1) (y+1))],[(Coordenada (x-1) (y+1))],[(Coordenada (x+1) y)],[(Coordenada (x-1) y)]]
 |(player == Sente) && (y==1) && (x==1) =[[(Coordenada x (y+1))],[(Coordenada (x+1) (y+1))],[(Coordenada (x+1) y)]]
 |(player == Sente) && (y==1) && (x==9) =[[(Coordenada x (y+1))],[(Coordenada (x-1) (y+1))],[(Coordenada (x-1) y)]]
 |(player == Sente) && (x==9) && (y/=1) && (y/=9) =[[(Coordenada x (y+1))],[(Coordenada (x-1) (y+1))],[(Coordenada (x-1) y)],[(Coordenada x (y-1))]]
 |(player == Sente) && (x==1) && (y/=1) && (y/=9) =[[(Coordenada x (y+1))],[(Coordenada (x+1) (y+1))],[(Coordenada (x+1) y)],[(Coordenada (x+1) (y-1))]]
 |(player == Gote) && (y==9) && (x/=1) && (x/=9) =[[(Coordenada x (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada x (y-1))],[(Coordenada (x-1) y)],[(Coordenada (x+1) y)]]
 |(player == Gote) && (y==9) && (x==1) =[[(Coordenada x (y-1))],[(Coordenada (x+1) (y-1))],[(Coordenada (x+1) y)]]
 |(player == Gote) && (y==9) && (x==9) =[[(Coordenada x (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x-1) y)]]
 |(player == Gote) && (y==1) && (x/=1) && (x/=9) =[[(Coordenada (x-1) y)],[(Coordenada (x+1) y)],[(Coordenada x (y+1))]]
 |(player == Gote) && (y==1) && (x==1) =[[(Coordenada x (y+1))],[(Coordenada (x+1) y)]]
 |(player == Gote) && (y==1) && (x==9) =[[(Coordenada (x-1) y)],[(Coordenada x (y+1))]]
 |(player == Gote) && (x==9) && (y/=1) && (y/=9) =[[(Coordenada x (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x-1) y)],[(Coordenada x (y+1))]]
 |(player == Gote) && (x==1) && (y/=1) && (y/=9) =[[(Coordenada x (y-1))],[(Coordenada (x+1) (y-1))],[(Coordenada (x+1) y)],[(Coordenada x (y+1))]]
 |(player == Sente) && (x/=1) && (x/=9) && (y/=1) && (y/=9) =[[(Coordenada (x+1) y)],[(Coordenada (x-1) y)],[(Coordenada (x+1) (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x+1) (y+1))],[(Coordenada (x-1) (y+1))],[(Coordenada x (y+1))]]
 |(player == Gote) && (x/=1) && (x/=9) && (y/=1) && (y/=9) =[[(Coordenada (x+1) y)],[(Coordenada (x-1) y)],[(Coordenada (x+1) (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x+1) (y+1))],[(Coordenada (x-1) (y+1))],[(Coordenada x (y-1))]]
 |otherwise = [[]]

caso1esMovimientoPosibleGeneralDorado = (esMovimientoPosibleGeneralDorado (GeneralDorado, (Coordenada 5 5), Sente))==[[(Coordenada 6 5)], [(Coordenada 4 5)], [(Coordenada 6 4)], [(Coordenada 4 4)], [(Coordenada 6 6)], [(Coordenada 4 6)], [(Coordenada 5 6)]]
caso2esMovimientoPosibleGeneralDorado = (esMovimientoPosibleGeneralDorado (GeneralDorado, (Coordenada 1 1), Sente)) == [[(Coordenada 1 2)], [(Coordenada 2 2)], [(Coordenada 2 1)]]

casosesMovimientoPosibleGeneralDorado = caso1esMovimientoPosibleGeneralDorado:caso2esMovimientoPosibleGeneralDorado:[]

todoBienesMovimientoPosibleGeneralDorado = and casosesMovimientoPosibleGeneralDorado

esMovimientoPosibleCaballo :: (Pieza, Coordenada, ShogiPlayer) -> [[Coordenada]]
esMovimientoPosibleCaballo (p, (Coordenada x y), player)
 |(player == Sente) && (y<=7) && (x/=1) && (x/=9) = [[(Coordenada (x+1) (y+2))],[(Coordenada (x-1) (y+2))]]
 |(player == Sente) && (y<=7) && (x==1) = [[(Coordenada (x+1) (y+2))]]
 |(player == Sente) && (y<=7) && (x==9) = [[(Coordenada (x-1) (y+2))]]
 |(player == Gote) && (y>=3) && (x/=1) && (x/=9) = [[(Coordenada (x+1) (y-2))],[(Coordenada (x-1) (y-2))]]
 |(player == Gote) && (y>=3) && (x==1) = [[(Coordenada (x+1) (y-2))]]
 |(player == Gote) && (y>=3) && (x==9) = [[(Coordenada (x-1) (y-2))]]
 |otherwise = [[]]

caso1esMovimientoPosibleCaballo = (esMovimientoPosibleCaballo (Caballo, (Coordenada 5 5), Sente))==[[(Coordenada 6 7)], [(Coordenada 4 7)]]

casosesMovimientoPosibleCaballo = caso1esMovimientoPosibleCaballo:[]

todoBienesMovimientoPosibleCaballo = and casosesMovimientoPosibleCaballo




esMovimientoPosibleRey :: (Pieza, Coordenada, ShogiPlayer) -> [[Coordenada]]
esMovimientoPosibleRey (p, (Coordenada x y), player) 
 |(y==9) && (x/=1) && (x/=9) = [[(Coordenada x (y-1))],[(Coordenada (x+1) (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x+1) y)],[(Coordenada (x-1) y)]]
 |(y==9) && (x==1) =[[(Coordenada x (y-1))],[(Coordenada (x+1) (y-1))],[(Coordenada (x+1) y)]]
 |(y==9) && (x==9) =[[(Coordenada x (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x-1) y)]]
 |(y==1) && (x/=1) && (x/=9) =[[(Coordenada (x+1) y)],[(Coordenada (x+1) (y+1))],[(Coordenada x (y+1))],[(Coordenada (x-1) (y+1))],[(Coordenada (x-1) y)]]
 |(y==1) && (x==1) =[[(Coordenada (x+1) y)],[(Coordenada (x+1) (y+1))],[(Coordenada x (y+1))]]
 |(y==1) && (x==9) =[[(Coordenada (x-1) y)],[(Coordenada (x-1) (y+1))],[(Coordenada x (y+1))]]
 |(x==9) && (y/=1) && (y/=9) =[[(Coordenada x (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x-1) y)],[(Coordenada (x-1) (y+1))],[(Coordenada x (y+1))]]
 |(x==1) && (y/=1) && (y/=9) =[[(Coordenada x (y-1))],[(Coordenada (x+1) (y-1))],[(Coordenada (x+1) y)],[(Coordenada (x+1) (y+1))],[(Coordenada x (y+1))]]
 |(x/=1) && (x/=9) && (y/=1) && (y/=9) = [[(Coordenada x (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x-1) y)],[(Coordenada (x-1) (y+1))],[(Coordenada x (y+1))],[(Coordenada (x+1) (y-1))],[(Coordenada (x+1) y)],[(Coordenada (x+1) (y+1))]]
 |otherwise = [[]]

caso1esMovimientoPosibleRey = (esMovimientoPosibleRey (Rey, (Coordenada 5 5), Sente))==[[(Coordenada 5 4)], [(Coordenada 4 4)], [(Coordenada 4 5)], [(Coordenada 4 6)], [(Coordenada 5 6)], [(Coordenada 6 4)], [(Coordenada 6 5)], [(Coordenada 6 6)]]

casosesMovimientoPosibleRey = caso1esMovimientoPosibleRey:[]

todoBienesMovimientoPosibleRey = and casosesMovimientoPosibleRey

esMovimientoPosibleGeneralPlateado :: (Pieza, Coordenada, ShogiPlayer) -> [[Coordenada]]
esMovimientoPosibleGeneralPlateado (p, (Coordenada x y), player)
 |(player == Sente) && (y==9) && (x/=1) && (x/=9) = [[(Coordenada (x-1) (y-1))],[(Coordenada (x+1) (y-1))]]
 |(player == Sente) && (y==9) && (x==1) = [[(Coordenada (x+1) (y-1))]]
 |(player == Sente) && (y==9) && (x==9) = [[(Coordenada (x-1) (y-1))]]
 |(player == Sente) && (y==1) && (x/=1) && (x/=9) = [[(Coordenada (x+1) (y+1))],[(Coordenada x (y+1))],[(Coordenada (x-1) (y+1))]]
 |(player == Sente) && (y==1) && (x==1) =[[(Coordenada (x+1) (y+1))],[(Coordenada x (y+1))]]
 |(player == Sente) && (y==1) && (x==9) =[[(Coordenada x (y+1))],[(Coordenada (x-1) (y+1))]]
 |(player == Sente) && (x==9) && (y/=1) && (y/=9) =[[(Coordenada x (y+1))],[(Coordenada (x-1) (y+1))],[(Coordenada (x-1) (y-1))]]
 |(player == Sente) && (x==1) && (y/=1) && (y/=9) =[[(Coordenada x (y+1))],[(Coordenada (x+1) (y+1))],[(Coordenada (x+1) (y-1))]]
 |(player == Gote) && (y==9) && (x/=1) && (x/=9) =[[(Coordenada x (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x+1) (y-1))]]
 |(player == Gote) && (y==9) && (x==1) =[[(Coordenada x (y-1))],[(Coordenada (x+1) (y-1))]]
 |(player == Gote) && (y==9) && (x==9) =[[(Coordenada x (y-1))],[(Coordenada (x-1) (y-1))]]
 |(player == Gote) && (y==1) && (x/=1) && (x/=9) =[[(Coordenada (x+1) (y+1))],[(Coordenada (x-1) (y+1))]]
 |(player == Gote) && (y==1) && (x==1) =[[(Coordenada (x+1) (y+1))]]
 |(player == Gote) && (y==1) && (x==9) =[[(Coordenada (x-1) (y+1))]]
 |(player == Gote) && (x==9) && (y/=1) && (y/=9) =[[(Coordenada x (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x-1) (y+1))]]
 |(player == Gote) && (x==1) && (y/=1) && (y/=9) =[[(Coordenada x (y-1))],[(Coordenada (x+1) (y-1))],[(Coordenada (x+1) (y+1))]]
 |(x/=1) && (x/=9) && (y/=1) && (y/=9) =[[(Coordenada (x+1) (y-1))],[(Coordenada x (y-1))],[(Coordenada (x-1) (y-1))],[(Coordenada (x+1) (y+1))],[(Coordenada (x-1) (y+1))]]
 |otherwise = [[]]

caso1esMovimientoPosibleGeneralPlateado = (esMovimientoPosibleGeneralPlateado (GeneralPlateado, (Coordenada 5 5), Sente)) == [[(Coordenada 6 4)], [(Coordenada 5 4)], [(Coordenada 4 4)], [(Coordenada 6 6)], [(Coordenada 4 6)]]

casosesMovimientoPosibleGeneralPlateado = caso1esMovimientoPosibleGeneralPlateado:[]

todoBienesMovimientoPosibleGeneralPlateado = and casosesMovimientoPosibleGeneralPlateado