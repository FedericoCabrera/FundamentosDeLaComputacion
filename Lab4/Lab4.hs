{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Lab4 where
import Prelude hiding ((||),(&&),Bool,True, False,null,length,sum,and,or,map,zip,zipWith,filter,any,all,(+),(*),(++),reverse,concat,replicate,min,foldr)

import Lab1
import Lab2
import Lab3

--Tipo de los Pares
--data (a,b) where { (,)::a->b->(a,b)}

null :: [a]->Bool
null = \xs -> case xs of {[]->True; z:zs->False}

length :: [a]->N
length = \xs -> case xs of {[]-> O; z:zs->S(length zs)}

duplicate::[a] -> [a]
duplicate = \xs -> case xs of { []->[]; z:zs-> z: z : duplicate zs }

sum :: [N]->N
sum = \xs -> case xs of {[]->O; z:zs-> z + sum(zs)}

prod :: [N]->N
prod = \xs -> case xs of {[]->O; z:zs-> z * sum(zs)}

map::(a->b) ->[a]-> [b]
map = \f xs -> case xs of {[]->[]; z:zs -> f z : map f zs }

zip::[a]-> [b]-> [(a,b)]
zip = \xs ws -> case xs of { [] -> []; z:zs -> case ws of { [] -> [] ; y:ys -> (z,y) : zip zs ys}}

zipWith::(a->b->c)-> [a]-> [b]-> [c]
zipWith = \f xs ws -> case xs of {
    [] -> [];
    z:zs -> case ws of { [] -> [];
                         y:ys -> f z y : zipWith f zs ys                       
    }
}

filter::(a->Bool) ->[a]-> [a]
filter = \f xs -> case xs of {
    [] -> [];
    z:zs -> case f z of {
        True -> z : filter f zs;
        False -> filter f zs
    }
}

and::[Bool]-> Bool
and = \xs -> case xs of { [] -> True; z:zs -> z && and (zs)}

or::[Bool]-> Bool
or = \xs -> case xs of { [] -> False; z:zs -> z || or (zs)}

cuantos::(a->Bool) ->[a]-> N
cuantos = \f xs -> case xs of { [] -> O ; z:zs -> case f z of {True -> S (cuantos f zs) ; False -> cuantos f zs }}

any::(a->Bool) ->[a]->Bool
any = \f xs -> case xs of { [] -> False ; z:zs -> case f z of {True -> True ; False -> any f zs }}

all::(a->Bool) ->[a]->Bool
all = \f xs -> case xs of { [] -> True ; z:zs -> case f z of {True -> all f zs ; False -> False }}

