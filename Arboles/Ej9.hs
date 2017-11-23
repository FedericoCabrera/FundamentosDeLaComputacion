{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Ej9 where

data BT where { U :: BT;
C :: BT;
P :: [BT] -> BT
M :: [BT] -> BT }

valor::BT -> Bool
valor = \t case t of {
    U -> True;
    C -> False;
    P ts -> and (map valor ts);
    M ts -> or (map valor ts) }
}



