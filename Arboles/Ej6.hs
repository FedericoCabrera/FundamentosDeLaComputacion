{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Ej6 where
    
data Exp where { Num :: Int ->Exp;
    Sum :: Exp -> Exp -> Exp;
    Mul :: Exp -> Exp -> Exp }
