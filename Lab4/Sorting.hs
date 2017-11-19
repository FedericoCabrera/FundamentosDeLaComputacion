{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Sorting where

--borrar1 :: Ord a => a -> [a] -> [a]
--borrar1 = \x l -> case l of {
--    [] -> [];
--    z:zs -> case x==z of { True } 
--} 

-- SelectSort:
-- ===========
    -- Selecciono el minimo elemento de la lista y lo pongo adelante de el resto de la lista
    -- que ya fue ordenada

minL :: Ord a => [a] -> a
minL = \l -> case l of { 
    [] -> error "lista vacia";
    [x] -> x;
    x:y:ys -> min x (minL (y:ys)) 
}
--rec. estructural EX-EX Llam rec. contenida

selectSort :: Ord a => [a] -> [a]
selectSort = \l -> case l of {
    [] -> [];
    z:zs -> minL l : selectSort (borrar1 (minL l) l)
}
-- rec. bien fundada: la llamada recursiva se hace sobre una lista que tiene un elemento menos que la lista
-- original


--MergeSort:
-- =========
    -- Se divide la lista en 2, repartiendo los elementos como un mazo de cartas (split:: [a]->([a],[a]))
    -- Se ordenan ambas listas (rec)
    -- Se intercalan los elementos de las listas ordenadas (merge::Ord a => [a]->[a]->[a])

split :: [a] -> ( [a],[a] )
split = \n -> case l of {
    [] -> ([],[]);
    [x] -> ([x],[]);
    x:y:ys -> (x: fst(split ys), y:snd(split ys))
}
-- rec. estructural. EX-EX Llam rec. contenida

merge :: Ord a => [a] -> [a] -> [a]
-- arma una lista ordenada con dos listas ordenadas
merge = \l1 l2 -> case l1 of {
    [] -> l2;
    x:xs -> case l2 of {
        [] -> l1;
        z:zs-> case x<z of {
            False -> z:merge l1 zs;
            True -> x:merge xs l2
        }
    }
}
-- rec. primitiva, sobre alguna de las dos listas

mergeSort :: Ord a => [a] -> [a]
mergeSort = \l -> case l of {
    [] -> [];
    [x] -> [x];
    x:z:zs -> case split l of { (xs,ys) -> merge (mergeSort xs) (mergeSort ys) }
}
-- rec. Bien fundada: en el caso x:z:zs el split me da dos listas mas chicas que la lista original 
-- (Tuvimos que agregar 2 casos base para asegurarnos de que esta condicion se cumple)

-- QuickSort:
-- ==========
    -- Separamos la lista poniendo los menores al primer elem (pivote) de un lado y los mayores o iguales del otro
    -- Ordenamos c/lista
    -- Unimos los menores (ordenados) el pivote y los mayores (ordenados)

quickSort:: Ord a => [a] -> [a]
quickSort = \l -> case l of { 
    [] -> [];
    z:zs -> quickSort (filter (<z) zs) ++ z:(filter (>=z) zs)
}
-- rec. bien fundada: las llamadas recursivas se hacen sobre filter (..) zs
-- que tiene menos elementos que z:zs