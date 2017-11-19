--Federico Cabrera 142835

{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Entregable3 where

type Conj a = [a]

-- a)
pert :: Eq a => a -> Conj a -> Bool
pert = \x xs -> case xs of { [] -> False ; z:zs -> case x == z of { True -> True ; False -> pert x zs} }

-- b)
esConj :: Eq a => [a] -> Bool
esConj = \xs -> case xs of { [] -> True ; z:zs -> case pert z zs of { True -> False ; False -> esConj zs } }

-- c)
incl :: Eq a => Conj a -> Conj a -> Bool
incl = \xs ys -> case xs of { [] -> True ; z:zs -> (pert z ys) && (incl zs ys) }

-- d)
inter :: Eq a => Conj a -> Conj a -> Conj a
inter = \xs ys -> case xs of { [] -> [] ; z:zs -> case pert z ys of { True -> z : inter zs ys ; False -> inter zs ys} }
