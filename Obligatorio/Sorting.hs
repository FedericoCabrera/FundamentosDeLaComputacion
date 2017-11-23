{-# OPTIONS_GHC -fno-warn-tabs #-}
module Sorting where

ordenada :: Ord a => [a] -> Bool
ordenada = \l -> case l of {[]-> True;
							[x]-> True;
							x:y:ys -> x<=y && ordenada (y:ys)}
-- rec. estructural: EX-EX, llam. rec c

perm ::  Eq a => [a] -> [a] -> Bool
perm = \l1 l2 -> case l1 of {[]-> null l2;
							x:xs -> elem x l2 && perm xs (borrar1 x l2)}
-- rec. primitiva en l1

borrar1 :: Eq a => a -> [a] -> [a]
borrar1 = \x l -> case l of {[]-> error "el elemento no esta en la lista";
							z:zs-> case x==z of {False-> z:borrar1 x zs;
												True-> zs}}
-- rec. primitiva en l

-- INSERTSORT ---
insert :: Ord a => a -> [a] -> [a]
insert = \x l -> case l of {[]-> [x];
							z:zs-> case x<z of {False-> z:insert x zs;
												True-> x:z:zs}}
-- rec. primitiva en l

insertSort :: Ord a => [a] -> [a]
insertSort = \l -> case l of {[]-> [];
							z:zs-> insert z (insertSort zs)} 
-- rec. primitiva en l

--- SELECT SORT ---
minL :: Ord a => [a] -> a
minL = \l -> case l of {[]-> error "lista vacia";
						[x] -> x;
						x:y:ys -> min x (minL (y:ys))}
-- rec. estructural. EX-EX. Llam rec. contenida

selectSort :: Ord a => [a] -> [a]
selectSort = \l -> case l of {[]-> [];
							z:zs-> minL l:selectSort (borrar1 (minL l) l)}
-- rec. bien fundada: la llamada reursiva se hace sobre una
-- lista que tiene un elemento menos que la lista original	


--- MERGE SORT ---
split :: [a] -> ([a],[a])
split = \l -> case l of {[]-> ([],[]);
						[x] -> ([x],[]);
						x:y:ys -> (x:fst(split ys),y:snd(split ys))}
-- rec. estructural. EX-EX. Llam. rec. contenida

merge :: Ord a => [a] -> [a] -> [a]
-- arma una lista ordenada con dos listas ordenadas 						
merge = \l1 l2 -> case l1 of {[]-> l2;
						x:xs -> case l2 of {[]-> l1;
									z:zs-> case x<z of {False->z:merge l1 zs;
														True ->x:merge xs l2}}}
-- rec. primitiva, sobre alguna de las dos listas
mergeSort:: Ord a => [a] -> [a]
mergeSort = \l -> case l of {[] -> [];
							[x] -> [x];
							x:z:zs -> case split l of 
							   {(xs,ys)-> merge (mergeSort xs) (mergeSort ys)}}
-- rec. Bien fundada: en el caso x:z:zs el split me da dos listas mas chicas
-- que la lista original (Tuvimos que agregar 2 casos base para asegurarnos
-- de que esta condicion se cumple)

-- QUICKSORT ---
quickSort:: Ord a => [a] -> [a]
quickSort = \l -> case l of {[] -> [];
					z:zs-> quickSort(filter(<z)zs)++ z:quickSort(filter(>=z)zs)}
-- rec. bien fundada: las llamadas recursivas se hacen sobre filter (..) zs
-- que tiene menos elementos que z:zs						
							
							
							
							
							
							
							
							
							
							
							
							
							
							
							
							


























	










												
