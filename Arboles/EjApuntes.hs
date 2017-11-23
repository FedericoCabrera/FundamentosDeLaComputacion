{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module EjApuntes where

data AB a where { Hoja::a -> AB a ; Nodo:: AB a -> AB a -> AB a }
    deriving Show

arbol1 :: AB Int 
arbol1 = Nodo ( Hoja 5) (Hoja 2)

arbol2 :: AB Int
arbol2 = Nodo ( Hoja 7) (Nodo (Hoja 3) (Nodo (Hoja 1) (Hoja 2) ))


cantHojas :: AB a -> Int
cantHojas = \t -> case t of {
    Hoja h -> 1;
    Nodo t1 t2 -> (cantHojas t1) + (cantHojas t2)  
}

cantNodos :: AB a -> Int
cantNodos = \t -> case t of {
    Hoja h -> 0;
    Nodo t1 t2 -> 1 + ((cantNodos t1) + cantNodos (t2))
}

hojas :: AB a -> [a]
hojas = \t -> case t of {
    Hoja h -> [h] ;
    Nodo t1 t2 -> (hojas t1) ++ (hojas t2) 
}

altura :: AB a -> Int
altura = \t -> case t of {
    Hoja h -> 1;
    Nodo t1 t2 -> case (altura t1 > altura t2) of {
        True -> 1+(altura t1) ;
        False -> 1+(altura t2)
    }
}

espejo :: AB a -> AB a
espejo = \t -> case t of {
    Hoja h -> Hoja h;
    Nodo t1 t2 -> Nodo (espejo t2) (espejo t1)
}

mapAB::(a->b) -> AB a -> AB b
mapAB = \f t -> case t of {
    Hoja h -> Hoja (f h);
    Nodo t1 t2 -> Nodo (mapAB f t1) (mapAB f t2)
}
