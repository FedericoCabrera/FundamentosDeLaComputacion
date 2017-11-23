{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
module ID3 where
import Datos
import Auxiliares
	
data ArBin a b where {Hoja :: b -> ArBin a b ; Nodo :: a -> (ArBin a b) -> (ArBin a b) -> (ArBin a b) }
  deriving Show
	
type ArDec = ArBin Atributo Clase


--12
id3 :: [Registro] -> [Atributo] -> Clase -> ArDec
id3 = undefined
--13
arbol :: ArDec
arbol = undefined
--14
clasificar :: ArDec -> (Nombre,[Bool]) -> (Nombre,Clase)
clasificar = undefined
clasificacion :: [(Nombre,Clase)]
clasificacion = undefined


