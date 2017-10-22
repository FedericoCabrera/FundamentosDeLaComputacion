{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Lab1 where
import Prelude(Show)
--Hacer ejercicios anteriores del laboratorio

data Bool where { False::Bool ; True::Bool }
 deriving Show

data Dia where { Lunes::Dia ; Martes::Dia ; Miercoles::Dia }
--

--Ej 5
f1 :: a -> (a -> b) -> b
f1 = \x-> \f -> f x

f2:: (a -> a) -> a -> a
f2 = \f -> \x -> f (f (f x))

f3:: a -> (a -> b) -> (b -> b -> c) -> c 
f3 = \x-> \y -> \z -> z (y x) (y x)

f5:: a -> b -> (b -> b ->c ) -> c
f5 = \x-> \y -> \z -> z y y

h1 :: (a -> b -> c) -> a -> b -> c
h1 = \f -> \x -> \y -> f x y

h2 :: (a -> b) -> (b -> c) -> a -> c
h2 = \f -> \g -> \x -> g (f x)

h3 :: (a -> b) -> (a -> b -> c) -> a -> c
h3 = \f -> \g -> \x -> g x (f x)

h4 :: (b -> c) -> (a -> c -> d) -> a -> b -> d
h4 = \f -> \g -> \x -> \y -> g x (f y)

not :: Bool -> Bool
not = \x-> case x of { False -> True ; True -> False }

(||) :: Bool -> Bool -> Bool
(||) = \x-> \y-> case x of { True -> True ; False -> case y of {True -> True ; False -> False} }

(&&) :: Bool -> Bool -> Bool 
(&&) = \x-> \y-> case x of { False -> case y of {False -> True ; True -> False} ; True -> case y of {True -> True ; False -> False} }

(>>) :: Bool -> Bool -> Bool
(>>) = \x-> \y-> case x of { True -> case y of { False -> False ; True -> True } ; False -> True}