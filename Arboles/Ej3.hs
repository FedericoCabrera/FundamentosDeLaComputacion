{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Ej3 where

data ABB a where { Vacio :: ABB a ; Unir :: ABB a -> a -> ABB a -> ABB a}
    deriving Show

arbol1:: ABB Int 
arbol1 = Unir ( Unir (Unir (Vacio) 3 (Vacio) ) 5 (Vacio) ) 7 ( Unir (Vacio) 4 (Vacio) )


inOrder:: ABB a -> [a]
inOrder = \t -> case t of {
    Vacio -> [];
    Unir a1 a a2 -> (inOrder a1)++(a:(inOrder a2))  
}

postOrder:: ABB a -> [a]
postOrder = \t -> case t of {
    Vacio -> [];
    Unir a1 a a2 -> ((postOrder a1)++(postOrder a2))++[a]  
}

pertenece::Eq a => a -> ABB a -> Bool
pertenece = \x t -> case t of {
    Vacio = False;
    Unir a1 a a2 -> case x==a {
        False -> (pertenece x a1) || (pertenece x a2) ;
        True -> True
    }
}