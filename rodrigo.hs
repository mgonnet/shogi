import Shogii

isFinished :: ShogiGame -> Bool
isFinished (ShogiGame _ a) = (length (filter (\(x,y,z) -> ((x==Rey)&&(y==(Coordenada 0 0))) ) a))==1

caso1 = (isFinished (ShogiGame (Just Sente) [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 3 3), Gote)]))==False
--Tengo dos reyes
caso2 = (isFinished (ShogiGame (Just Sente) [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 3 3), Gote), (Caballo, (Coordenada 3 3), Gote)]))==False
--Tengo dos reyes y un caballo
caso3 = (isFinished (ShogiGame (Just Sente) [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 0 0), Sente), (Caballo, (Coordenada 3 3), Gote)]))==True
--Tengo un rey y un caballo
caso4 = (isFinished (ShogiGame (Just Sente) [(Rey, (Coordenada 3 2), Sente), (Rey, (Coordenada 0 0), Sente)]))==True
--Tengo solo un rey

casos = caso1:caso2:caso3:caso4:[]

todoBien = and casos