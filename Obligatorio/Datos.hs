{-# OPTIONS_GHC -fno-warn-tabs #-}
module Datos where

data Atributo = Fiebre | Moco | Rubor | DolorArtic | DolorCabeza
	deriving (Eq,Ord,Enum,Show)
		
data Clase = Enfermo | Sano | Incubando
	deriving  (Eq,Ord,Enum,Show)
	
type Nombre = String
	
type Registro = (Nombre,Clase,[Bool])
type Datos = [Registro]

ejs :: Datos
ejs = [ ("Diego",Enfermo,[True,True,True,True,True]),
		("Juan",Sano,[False,False,False,False,False]),
		("Manuela",Incubando,[False,True,True,False,True]),
		("Nora",Sano,[False,True,False,False,True]),
		("JPablo",Enfermo,[True,True,False,False,True]),
		("Lucia",Enfermo,[True,False,False,False,True]),
		("Valeria",Enfermo,[True,False,False,True,False]),
		("Joaquin",Enfermo,[False,True,False,True,False])]

-- el resultado de arDec deberá ser:
-- Nodo Fiebre (Nodo Rubor (Nodo DolorCabeza (Nodo DolorArtic (Hoja Sano) (Hoja Enfermo)) (Hoja Sano)) (Hoja Incubando)) (Hoja Enfermo)
	
	
tests :: [(Nombre,[Bool])]
tests = [("Mauricio",[False,True,True,False,True]),
		("Federico",[False,False,False,False,True]),
		("Igor",[False,True,False,True,True]),
		("Bruno",[True,True,False,False,False]),
		("Ignacio",[False,True,False,True,False])]
		
-- la clasificacion de estos tests deberá ser:
-- [("Mauricio",Incubando),("Federico",Sano),("Igor",Sano),("Bruno",Enfermo),("Ignacio",Enfermo)]
