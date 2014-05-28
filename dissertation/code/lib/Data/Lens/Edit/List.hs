{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}

module Data.Lens.Edit.List where

import Data.Lens.Bidirectional
import Data.Lens.Edit.Stateful (C) -- needed for GHC 7.2
import Data.List
import Data.Module.Class
import Data.Module.List
import Data.Module.Product
import Data.Module.Sum
import Data.Monoid
import qualified Data.Lens.Edit.Stateful  as F -- state_f_ul
import qualified Data.Lens.Edit.Stateless as L -- state_l_ess

data Map l = Map l deriving (Eq, Ord, Show, Read)

instance Bidirectional l => Bidirectional (Map l) where
	type L (Map l) = [ListAtom (L l)]
	type R (Map l) = [ListAtom (R l)]

instance F.Lens l => F.Lens (Map l) where
	type C (Map l) = [C l]
	missing _ = []
	dputr (Map l) = F.foldState (dputMapF l F.dputr)
	dputl (Map l) = F.foldState (dputMapF l F.dputl)

dputMapF l dput e cs = case e of
	FailList    -> ([FailList], cs)
	Modify p dx -> case split3 p cs of
		Just (b, c, e) -> let (dy, c') = dput l (dx, c)
		                  in  ([Modify p dy], b ++ [c'] ++ e)
		Nothing        -> ([FailList], cs)
	Insert i    -> ([Insert i], cs ++ genericReplicate i (F.missing l))
	Delete i    -> ([Delete i], zipWith const cs (genericDrop i cs))
	Rearrange p -> ([Rearrange p], applyPermutation p cs)

instance L.Lens l => L.Lens (Map l) where
	dputr (Map l) = map (dputMapL (L.dputr l))
	dputl (Map l) = map (dputMapL (L.dputl l))

dputMapL dput e = case e of
	{-
	Modify p dx -> Modify p (dput dx)
	_ -> unsafeCoerce e
	-}
	FailList    -> FailList
	Modify p dx -> Modify p (dput dx)
	Insert i    -> Insert i
	Delete i    -> Delete i
	Rearrange p -> Rearrange p

data Partition dX dY = Partition deriving (Eq, Ord, Show, Read)
instance Bidirectional (Partition dX dY) where
	type L (Partition dX dY) = [ListAtom [SumAtom dX dY]]
	type R (Partition dX dY) = [ProductAtom [ListAtom dX] [ListAtom dY]]

instance (Monoid dX, Monoid dY) => F.Lens (Partition dX dY) where
	type C (Partition dX dY) = [Tag]
	missing _ = []
	dputr _ = F.foldState dputrPartition
	dputl _ = F.foldState dputlPartition

dputrPartition (Modify p dvs) c
	| 1 <= p && p <= n = F.foldState (dputrPartitionMod p n) (dvs, c)
	| otherwise        = (failPartition, c)
	where n = genericLength c
dputrPartition (Insert i) c = ([Left [Insert i]], c ++ genericReplicate i L)
dputrPartition (Delete i) c = ([d1, d0], del i c) where
	del i c  = zipWith const c (genericDrop i c)
	(nL, nR) = count (i+1) (reverse c)
	d0 = Left  [Delete (nL-1)]
	d1 = Right [Delete (nR-1)]
dputrPartition (Rearrange p) c = ([dL, dR], c') where
	n        = genericLength c
	(nL, nR) = count (n+1) c
	c'       = applyPermutation p c
	dL       = Left  [Rearrange (Simple iL)]
	dR       = Right [Rearrange (Simple iR)]
	iL       = map (out . h . Left ) [1..nL-1]
	iR       = map (out . h . Right) [1..nR-1]
	out      = either id id
	h        = iso c . complexPermutation p n . isoInv c'
dputrPartition FailList c = (failPartition, c)

dputrPartitionMod p n e c = case e of
	FailSum     -> (failPartition, c)
	StayL dv    -> ([Left  [Modify pL dv]], c)
	StayR dv    -> ([Right [Modify pR dv]], c)
	SwitchLL dv -> ([Left  [Modify pL dv], Left  (ins pL n), Left  (del pL n)], set L)
	SwitchLR dv -> ([Right [Modify pR dv], Right (ins pR n), Left  (del pL n)], set R)
	SwitchRL dv -> ([Left  [Modify pL dv], Left  (ins pL n), Right (del pR n)], set L)
	SwitchRR dv -> ([Right [Modify pR dv], Right (ins pR n), Right (del pR n)], set R)
	where
	(pL, pR) = count p c
	set v    = case split3 p c of
		Just (b, _, e) -> b ++ [v] ++ e
		Nothing        -> c

dputlPartition (Left  dvs) c = F.foldState (dputlPartition' L StayL SwitchLL onL) (dvs, c)
dputlPartition (Right dvs) c = F.foldState (dputlPartition' R StayR SwitchLR onR) (dvs, c)

dputlPartition' j stayj switchLj onj e c = case e of
	Modify p dx
		| 1 <= p && p <= n -> ([Modify p' [stayj dx]], c)
		| otherwise        -> ([FailList], c)
		where
		n  = genericLength c
		p' = isoInv c (tag j p)
	Insert i -> (switches ++ insert, c ++ genericReplicate i j) where
		n        = genericLength c
		switches = [Modify p [switchLj mempty] | p <- [n+1 .. n+i]]
		insert   = [Insert i]
	Delete i -> F.foldState dputlPartitionDelete (genericReplicate i j, c)
	Rearrange p -> ([Rearrange (Simple is)], c) where
		nj = sum [1 | k <- c, j == k]
		g  = onj (complexPermutation p nj)
		f  = isoInv c . g . iso c
		is = map f [1..length c]
	FailList -> ([FailList], c)

dputlPartitionDelete j c = (del p n, c') where
	n  = genericLength c
	nj = sum [1 | k <- c, j == k]
	p  = isoInv c (tag j nj)
	c' = case split3 p c of
		Just (b, _, e) -> b ++ e

failPartition = [Left [FailList], Right [FailList]]

-- TODO: implement in terms of functions like length/filter
count p []     = (1,1)
count 1 cs     = (1,1)
count p (c:cs) = (nL + isL, nR + isR) where
	(nL, nR) = count (p-1) cs
	(isL, isR) = case c of L -> (1,0); R -> (0,1)

iso c p = case genericIndex c (p-1) of
	L -> Left  (fst (count p c))
	R -> Right (snd (count p c))

label = go (1,1) where
	go (l,r) []     = []
	go (l,r) (L:xs) = Left  l : go (l+1,r) xs
	go (l,r) (R:xs) = Right r : go (l,r+1) xs

isoInv c p = case findIndex (p==) (label c) of
	Nothing -> error $  "went out of bounds while trying to compute index "
	                 ++ show p
	                 ++ " in complement "
	                 ++ show c
	Just i  -> toInteger i+1

ins p n = [Rearrange (Simple ([1 .. p-1] ++ [n+1] ++ [p+1 .. n])), Insert 1]
del p n = [Delete 1, Rearrange (Simple ([1 .. p-1] ++ [p+1 .. n] ++ [p]))]

tag L = Left
tag R = Right

onL f = either (Left . f) Right
onR f = either Left (Right . f)
