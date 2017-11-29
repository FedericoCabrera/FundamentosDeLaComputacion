{-# OPTIONS_GHC -fno-warn-tabs #-}
module Auxiliares where
import Datos
	
--1
atributos :: [Atributo]
atributos = [toEnum 0 ..]

clases :: [Clase]
clases = [toEnum 0 ..]

--2
--Funciones auxiliares para obtener cada uno de los valores de la terna registro

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
valorAtr = \a (n,c,bs) -> bs!!(fromEnum a)

--4
regsAtrB :: Atributo -> Bool -> [Registro] -> [Registro]
regsAtrB = \a b rs ->  case rs of {
  [] -> [];
  x:xs -> case b==(valorAtr a x) of {
    True -> x:regsAtrB a b xs;
    False -> regsAtrB a b xs
  }
}
--5	
regsClaseAux :: Clase -> Registro -> Bool
regsClaseAux = \c1 (n,c2,bs) -> c1 == c2

regsClase :: Clase -> [Registro] -> [Registro]
regsClase = \c rs -> case rs of {
  [] -> [];
  x:xs -> case regsClaseAux c x of {
    True -> x:regsClase c xs;
    False -> regsClase c xs
  } 
}

--6	
prop :: Clase -> Atributo -> [Registro] -> Float
prop = \c a rs -> (fromIntegral (length( regsAtrB a True (regsClase c rs)) ) ) / fromIntegral ( length(regsClase c rs) )

--7
entrAtrClase :: Clase -> Atributo -> [Registro] -> Float
entrAtrClase = \c a rs -> case (prop c a rs) /= 0 of {
  True -> (prop c a rs * log(prop c a rs));
  False -> 0
}

--8
--Funcion auxiliar para aplicar entrAtrClase a una lista de clases
entrAtrAux :: [Clase] -> Atributo -> [Registro] -> Float
entrAtrAux = \cs a rs -> case cs of {
  [] -> 0.0;
  x:xs -> (entrAtrClase x a rs) + (entrAtrAux xs a rs)
}

entrAtr :: [Atributo] -> [Registro] -> Atributo -> Float
entrAtr = \ats rs a -> case ats of {
  [] -> 1.0;
  x:xs -> case x == a of {
    True -> -1 * (entrAtrAux clases a rs);
    False -> entrAtr xs rs a
  }
}

--9
--Funcion auxiliar que recibe dos atributos y calcula el menor segun su entropia
minEntrAtr :: [Atributo] -> [Registro] -> Atributo -> Atributo -> Atributo
minEntrAtr = \ats rs a1 a2 -> case (entrAtr ats rs a2) < (entrAtr ats rs a1) of {
  True -> a2;
  False -> a1;
}

minAtr :: [Atributo] -> [Registro] -> Atributo
minAtr = \ats rs -> case ats of {
  [] -> error "lista vacia";
  [x] -> x;
  x:y:ys -> minEntrAtr ats rs x (minAtr (y:ys) rs)
}

--10		
--Funciones auxiliares para agregar un registro a la primera o segunda componente de la dupla	
partAtrFstAux :: Registro -> ([Registro],[Registro]) -> ([Registro],[Registro])
partAtrFstAux = \r rs -> case rs of {(x,y) -> (r:x,y)}

partAtrSndAux :: Registro -> ([Registro],[Registro]) -> ([Registro],[Registro])
partAtrSndAux = \r rs -> case rs of {(x,y) -> (x,r:y)}

partAtr :: Atributo -> [Registro] -> ([Registro],[Registro])
partAtr = \a rs -> case rs of {
  [] -> ([],[]);
  x:xs -> case valorAtr a x of {
    False -> partAtrFstAux x (partAtr a xs) ;
    True -> partAtrSndAux x (partAtr a xs)
  }
}

--11
--Funcion auxiliar que verifica el maximo de ocurrencias, dada una lista de asociaciones Clase,Ocurrencias
--(teniendo en cuenta que siempre voy a tener 3 clases)
maxOcurrencias :: [(Clase,Int)] -> Clase
maxOcurrencias = \cs -> case cs of {
  [] -> error "lista vacia";
  x:y:z:xs -> case (snd x) > (snd y) of { 
    True -> case (snd x) > (snd z) of {
      True -> fst x; 
      False -> fst z
      };
    False -> case (snd y) > (snd z) of {
      True-> fst y; 
      False-> fst z
      }
    }
}

maxClase :: [Registro] -> Clase
maxClase = \rs -> maxOcurrencias (ocurrencias rs)

--Funcion auxiliar para guardar la ocurrencia de una clase segun un registro
incrementarRegistro :: Registro -> [(Clase,Int)] -> [(Clase,Int)]
incrementarRegistro = \r cs -> case cs of {
  [] -> [(Enfermo,0),(Sano,0),(Incubando,0)] ;
  x:xs -> case (sndReg r) of {
    Enfermo -> [ (Enfermo,((snd(cs!!0))+1)) , (Sano,(snd(cs!!1))) , (Incubando,(snd(cs!!2))) ];
    Sano -> [ (Enfermo,(snd(cs!!0))) , (Sano,(snd(cs!!1)+1)) , (Incubando,(snd(cs!!2))) ];
    Incubando -> [ (Enfermo,((snd(cs!!0)))) , (Sano,(snd(cs!!1))) , (Incubando,(snd(cs!!2)+1)) ]
  }
}

ocurrencias :: [Registro] -> [(Clase,Int)]
ocurrencias = \rs -> case rs of {
  [] -> [(Enfermo,0),(Sano,0),(Incubando,0)] ;
  x:xs -> incrementarRegistro x (ocurrencias xs)
}

