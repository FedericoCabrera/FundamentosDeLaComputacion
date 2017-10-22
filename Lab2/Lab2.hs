{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Lab2 where
import Prelude(Show)
import Lab1

data Carga where { Positivo::Carga ; Negativo::Carga } deriving Show

opuesto :: Carga -> Carga
opuesto= \x -> case x of { Positivo->Negativo ; Negativo->Positivo } 

data Dia where {Lu::Dia;Ma::Dia;Mi::Dia;Ju::Dia;Vi::Dia;Sa::Dia;Do::Dia} deriving Show

siguiente :: Dia -> Dia
siguiente= \x -> case x of { Lu -> Ma ; Ma -> Mi ; Mi -> Ju ; Ju -> Vi ; Vi -> Sa ; Sa -> Do ; Do -> Lu}

laborable::Dia -> Bool
laborable= \x -> case x of { Lu -> True ; Ma -> True ; Mi -> True ; Ju -> True ; Vi -> True ; Sa -> False ; Do -> False}

laborable2::Dia -> Bool
laborable2= \x -> case x of { Sa -> False ; Do -> False ; otherwise -> True}


--Ej 3
data PPT where {Piedra::PPT;Papel::PPT;Tijera::PPT} deriving Show

gana::PPT -> PPT -> PPT
gana= \x -> \y -> case x of { 
Piedra -> case y of {
    Papel -> Papel; Piedra -> x; Tijera -> Piedra;
};
Papel -> case y of {
    Papel -> x; Piedra -> Papel; Tijera -> Tijera
};
Tijera -> case y of {
    Papel -> Tijera; Piedra -> Piedra; Tijera -> x
}
} 

--Clases--

--Ej 4

flip         :: (a -> b -> c) -> b -> a -> c
flip f x y   =  f y x

class Eq a where
    { (==),(/=)::a->a->Bool ;
    (/=) = \x y -> not (x==y) }
class Eq a => Ord a where
    { (<=),(<),(>=),(>)::a->a->Bool ;
    (>) = \x y -> not (x <= y) ;
    (>=) = \x y -> (x > y) || (x == y);
    (<) = flip (>)}
    
--Ej 5

instance Eq Bool where
 (==) = \x-> \y-> case x of { True -> y ; False -> not y } 

instance Ord Bool where
 (<=) = \x y -> case x of { False -> True ; True -> y }

 --Ej 6

 instance Eq PPT where
    (==) = \x y -> case x of { Tijera -> case y of { Tijera -> True ; otherwise -> False} 
    Piedra -> case y of { Piedra -> True ; otherwise -> False}
    Papel -> case y of { Papel -> Ture ; otherwise -> False}}

instance Ord PPT where
    (<=) = \x y -> case x { Piedra -> case y { Papel -> True ; otherwise -> True }}   