{-# OPTIONS_GHC -fno-warn-tabs #-}
{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
module ID3 where
import Datos
import Auxiliares
	
data ArBin a b where {Hoja :: b -> ArBin a b ; Nodo :: a -> (ArBin a b) -> (ArBin a b) -> (ArBin a b) }
  deriving Show
	
type ArDec = ArBin Atributo Clase


--12
-- Nodo Fiebre (Nodo Rubor (Nodo DolorCabeza (Nodo DolorArtic (Hoja Sano) (Hoja Enfermo)) (Hoja Sano)) (Hoja Incubando)) (Hoja Enfermo)
--Funcion auxiliar para quitar atributo de la lista
obtRsTrue :: [Atributo] -> [Registro] -> [Registro]
obtRsTrue = \as rs ->  fst (partAtr ( minAtr as rs ) rs)

obtRsFalse :: [Atributo] -> [Registro] -> [Registro]
obtRsFalse = \as rs ->  snd (partAtr ( minAtr as rs ) rs)

obtAsFiltrado :: [Atributo] -> [Registro] -> [Atributo]
obtAsFiltrado = \as rs -> (filter(\e -> e /= (minAtr as rs)) as)

id3 :: [Registro] -> [Atributo] -> Clase -> ArDec
id3 = \rs as c -> case rs of {
  [] -> Hoja c;
  x:xs -> case as of {
    --[] -> Hoja c;
    y:ys -> Nodo ( minAtr as [x] ) ( id3 (obtRsFalse (obtAsFiltrado as xs) xs) (obtAsFiltrado as xs)  (maxClase (obtRsFalse (obtAsFiltrado as xs) xs)) ) ( id3 (obtRsTrue (obtAsFiltrado as xs) xs) (obtAsFiltrado as xs)  (maxClase (obtRsTrue (obtAsFiltrado as xs) xs)) ) 
      --Nodo (id3  (fst  (partAtr ( minAtr as rs ) rs)) (filter(\e -> e /= (minAtr as rs)) as) (maxClase (fst  (partAtr ( minAtr as rs ) rs)))  )
    --Nodo (id3  (snd  (partAtr ( minAtr as rs ) rs)) (filter(\e -> e /= (minAtr as rs)) as) (maxClase (snd  (partAtr ( minAtr as rs ) rs)))  )


  } 
}
--13
arbol :: ArDec
arbol = id3 ejs atributos (maxClase ejs)
--14
clasificar :: ArDec -> (Nombre,[Bool]) -> (Nombre,Clase)
clasificar = undefined
clasificacion :: [(Nombre,Clase)]
clasificacion = undefined


