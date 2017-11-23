{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Ej13 where

data A where { H :: A;
U :: A -> A ;
T :: A -> A -> A -> A }

nodos :: A -> Int
nodos = \a -> case a of {
    H ts -> 1 + nodos ts;
    U ts -> 1 + nodos ts + nodos ts;
}