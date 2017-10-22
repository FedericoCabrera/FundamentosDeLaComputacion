{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Lab1 where
import Prelude(Show)
--Hacer ejercicios anteriores del laboratorio

data Bool where { False::Bool ; True::Bool }
 deriving Show

--

--Ej 5
f1 :: a -> (a -> b) -> b
f1 = \x-> \f -> f x

--f2:: (a -> a) -> a -> (a -> (a -> (a -> a)))
--f2 = \f -> \x -> f (f (f x))

not :: Bool -> Bool
not = \x-> case x of { False -> True ; True -> False }

(||) :: Bool -> Bool -> Bool
(||) = \x-> \y-> case x of { True -> True ; False -> case y of {True -> True ; False -> False} }

(&&) :: Bool -> Bool -> Bool 
(&&) = \x-> \y-> case x of { False -> case y of {False -> True ; True -> False} ; True -> case y of {True -> True ; False -> False} }

(>>) :: Bool -> Bool -> Bool
(>>) = \x-> \y-> case x of { True -> case y of { False -> False ; True -> True } ; False -> True}