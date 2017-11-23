{-# OPTIONS_GHC -fno-warn-tabs #-}
module Auxiliares where
import Datos
	
--1
atributos :: [Atributo]
atributos = [toEnum 0 ..]

clases :: [Clase]
clases = [toEnum 0 ..]

--2
--Ejemplo: clase "Nora" ejs = Sano

fstReg :: Registro -> Nombre
fstReg = \r -> case r of { (x,y,z) -> x }

sndReg :: Registro -> Clase
sndReg = \r -> case r of { (x,y,z) -> y }

trdReg :: Registro -> [Bool]
trdReg = \r -> case r of { (x,y,z) -> z }

clase :: Nombre -> [Registro] -> Clase
clase = \n r -> case r of {
  [] -> error "lista vacia" ;
  x:xs -> case fstReg(x) == n of {
    True -> sndReg x;
    False -> clase n xs
  } 
}

--3
valorAtr :: Atributo -> Registro -> Bool
valorAtr = \a (n,c,bs) -> undefined
--4
regsAtrB :: Atributo -> Bool -> [Registro] -> [Registro]
regsAtrB = undefined
--5	
regsClase :: Clase -> [Registro] -> [Registro]
regsClase = undefined
--6	
prop :: Clase -> Atributo -> [Registro] -> Float
prop = undefined
--7
entrAtrClase :: Clase -> Atributo -> [Registro] -> Float
entrAtrClase = undefined
--8
entrAtr :: [Atributo] -> [Registro] -> Atributo -> Float
entrAtr = undefined
--9
minAtr :: [Atributo] -> [Registro] -> Atributo
minAtr = undefined
--10				
partAtr :: Atributo -> [Registro] -> ([Registro],[Registro])
partAtr = undefined
--11
maxClase :: [Registro] -> Clase
maxClase = undefined
ocurrencias :: [Registro] -> [(Clase,Int)]
ocurrencias = undefined			

