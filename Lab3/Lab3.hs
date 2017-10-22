{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Lab3 where
import Prelude(Show)
import Lab1
import Lab2

data N where {O::N ; S::N -> N} deriving Show

uno::N
uno = S O
dos::N
dos = S uno
tres::N
tres = S dos
cuatro::N
cuatro = S tres
cinco::N
cinco = S cuatro

sumar::Dia->N->Dia
sumar= \d n -> case n of { O -> d; S x -> sumar (siguiente d) x}

pos::N -> Bool
pos = \n-> case n of 
    {O -> False;
    S x -> True }

--Ejercicio 1
pred :: N -> N
pred = \n -> case n of {
    O -> O;
    S x -> x
}

--Ejercicio 2
par :: N -> Bool
par = \n -> case n of {
            O -> True;
            S x -> not (par x) }


--Ejercicio 3
impar :: N -> Bool
impar = \n -> not (par n)

--Ejercicio 4
doble::N -> N
doble = \n -> case n of {
    O -> O;
    S x -> S(S(doble x));
}



--doble :: N -> N
--doble = \n -> case n of {
 --           O -> O;
  --          S x -> S (S (doble x))
   --         }

--Ejercicio 5
triple :: N -> N
triple = \n -> case n of {
        O -> O;
        S x -> S (S (S (triple x)))
    }

--Ejercicio 6
existe:: N->(N->Bool)->Bool
existe = \n p -> case n of {
    O -> p O;
    S x -> case p x of { True -> True; False -> existe x p} 
}

--Prueba
esTres:: N -> Bool
esTres = \n -> case n of {S ( S ( S (O))) -> True ; otherwise -> False}

--Ejercicio 7
todos:: N->(N->Bool)->Bool
todos = \n p -> case n of {
    O -> p O;
    S x -> p (S x) && todos x p } 

todos2:: N->(N->Bool)->Bool
todos2 = \n p -> case n of {
    O -> p O;
    S x -> case p x of {True -> todos2 x p ; False -> False}} 

--Ejercicio 8
contar:: N->(N->Bool)->N
contar = \n p -> case n of {
    O -> case p O of { True -> S O ; False -> O} ;
    S x -> case p (S x) of { True -> S ( contar x p) ; False -> contar x p}
}

--Ejercicio 9
(+)::N->N->N
(+)= \n m -> case n of {
    O -> m;
    S x -> S ( x + m)
}

(*)::N->N->N
(*) = \n m -> case n of {
    O -> O;
    S x -> m + (x * m)
}


(^)::N->N->N
(^)= \m n -> case n of { 
    O-> uno ;
    S x -> m * ( m^x )
}
--(+)::N->N->N
--(+) = \n m -> case n of { 
 --   O -> m;
 --   S k -> case m of {O -> S k ; S j -> S ( (S k) + j ) } }

--(*)::N->N->N
--(*) = \n m -> case n of {
--    O -> O;
--    S k -> case m of { O -> O ; S j ->  (S k) + (S k * j) }
--}

--(^)::N->N->N
--(^) = \n m -> case n of {
--    O -> O;
--    S k -> case m of { O -> uno ; S j ->  (S k) * (S k ^ j) }
--}

--Ejercicio 10
fact::N->N
fact = \n -> case n of {
    O -> uno;
    S k -> S k * (fact k) 
}            

--Ejercicio 11
sumi::N->N
sumi = \n -> case n of {
    O -> O;
    S k -> S k + ( sumi k )
}

--Ejercicio 12
sumdobles::N->N
sumdobles = \n -> case n of {
    O -> O;
    S k -> doble(S k) + ( sumdobles k)
}

--Ejercicio 13
sumfi::N->(N->N)->N
sumfi = \n f -> case n of {
    O -> f O;
    S k -> f(S k) + sumfi k f
}

--Ejercicio 14
sumpares::N->N
sumpares = \n -> case n of {
    O -> S O;
    S k -> case par (S k) of {
        True -> S (sumpares k);
        False -> sumpares k
    } 
}

--Ejercicio 15
sumpi::N->(N->Bool)->N
sumpi = \n f -> case n of {
    O -> case f O of {
        True -> S O;
        False -> O
    };
    S k -> case f (S k) of {
        True -> S ( sumpi k f );
        False -> sumpi k f
    }
}

--Ejercicio 16
instance Eq N where 
    (==) = \n m -> case n of {
        O -> case m of{
            O -> True;
            S k -> False
        };
        S k -> case m of {
            O -> False;
            S j -> k == j
        }
    }

--Ejercicio 17
instance Ord N where 
    (<=) = \n m -> case n of {
        O -> True ;
        S k -> case m of {
            O -> False;
            S j -> k <= j
        }
    }