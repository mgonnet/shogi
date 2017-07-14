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

movimientoSPosiblesLancero :: (Pieza, Coordenada, ShogiPlayer) -> [[Coordenada]]
movimientoSPosiblesLancero (pieza, (Coordenada columna fila), Sente) = if (fila==9) then [[]] else [[(Coordenada columna fila)| fila <-[(fila+1)..9]]]
movimientoSPosiblesLancero (pieza, (Coordenada columna fila), Gote) = if (fila==1) then [[]] else [[(Coordenada columna fila)| fila <- ([(fila-1),(fila-2)..1])]]