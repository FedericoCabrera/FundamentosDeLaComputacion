{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Ej10 where

data LArbol a where { Nodo::a -> [LArbol a] -> LArbol a}

cantNodosLA::LArbol a -> Int
cantNodosLA = \la -> case la of {
    Nodo x l -> 1 + sum (map cantNodosLA l)
}

altura::LArbol a -> Int
altura = \t -> case t of {
    Nodo x ts -> case ts of {
        [] -> 0;
        _ -> 1 + maximum (map altura ts)
    }
}

mapLA::(a->b) -> LArbol a -> LArbol b
mapLA = \f t -> case t of {
    Nodo x ts -> Nodo (f x) (map (mapLA f) ts)
}

aridad::LArbol a -> Int
aridad = \t -> case t of {
    Nodo x ts -> maximum (length ts : map aridad ts)
}

larbol2list::LArbol a -> [a] 
larbol2list = \t -> case t of { 
    Nodo x ts -> case ts of {
        case ts of {
            []-> [x];
            t1:ts1 -> LArbol t1 ++ larbol2list ( Nodo x ts1 )
        }
    } 
}