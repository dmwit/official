{-# LANGUAGE TypeFamilies #-}
module Data.Lens.Edit.Sum where

import Data.Lens.Bidirectional
import Data.Lens.Edit.Stateful (C) -- needed for GHC 7.2
import qualified Data.Lens.Edit.Stateful  as F -- state_f_ul
import qualified Data.Lens.Edit.Stateless as L -- state_l_ess
import qualified Data.Module.Sum          as M -- _m_odule

data CompactSum k l = CompactSum k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l) => Bidirectional (CompactSum k l) where
	type L (CompactSum k l) = M.Sum (L k) (L l)
	type R (CompactSum k l) = M.Sum (R k) (R l)

instance (F.Lens k, F.Lens l) => F.Lens (CompactSum k l) where
	type C  (CompactSum k l) = (F.C k, F.C l)
	missing (CompactSum k l) = (F.missing k, F.missing l)
	dputr (CompactSum k l) (M.Sum f dx dz, (ck, cl))
		= (M.Sum (M.retype f) dy dw, (ck', cl'))
		where
		(dy, ck') = F.dputr k (dx, M.bool f ck (F.missing k))
		(dw, cl') = F.dputr l (dz, M.bool f cl (F.missing l))
	dputl (CompactSum k l) (M.Sum f dy dw, (ck, cl))
		= (M.Sum (M.retype f) dx dz, (ck', cl'))
		where
		(dx, ck') = F.dputl k (dy, M.bool f ck (F.missing k))
		(dz, cl') = F.dputl l (dw, M.bool f cl (F.missing l))

instance (L.Lens k, L.Lens l) => L.Lens (CompactSum k l) where
	dputr (CompactSum k l) (M.Sum f dx dz)
		= M.Sum (M.retype f) (L.dputr k dx) (L.dputr l dz)
	dputl (CompactSum k l) (M.Sum f dy dw)
		= M.Sum (M.retype f) (L.dputl k dy) (L.dputl l dw)

data SumFL k l = SumFL k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l) => Bidirectional (SumFL k l) where
	type L (SumFL k l) = M.Sum (L k) (L l)
	type R (SumFL k l) = M.Sum (R k) (R l)

instance (F.Lens k, L.Lens l) => F.Lens (SumFL k l) where
	type C  (SumFL k l) = F.C k
	missing (SumFL k l) = F.missing k
	dputr (SumFL k l) (M.Sum f dx dz, ck) =
		let (dy, ck') = F.dputr k (dx, M.bool f ck (F.missing k))
		in (M.Sum (M.retype f) dy (L.dputr l dz), ck')
	dputl (SumFL k l) (M.Sum f dy dw, ck) =
		let (dx, ck') = F.dputl k (dy, M.bool f ck (F.missing k))
		in (M.Sum (M.retype f) dx (L.dputl l dw), ck')

data CompactSumLF k l = CompactSumLF k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l) => Bidirectional (CompactSumLF k l) where
	type L (CompactSumLF k l) = M.Sum (L k) (L l)
	type R (CompactSumLF k l) = M.Sum (R k) (R l)

instance (L.Lens k, F.Lens l) => F.Lens (CompactSumLF k l) where
	type C  (CompactSumLF k l) = F.C l
	missing (CompactSumLF k l) = F.missing l
	dputr (CompactSumLF k l) (M.Sum f dx dz, cl) =
		let (dw, cl') = F.dputr l (dz, M.bool f cl (F.missing l))
		in (M.Sum (M.retype f) (L.dputr k dx) dw, cl')
	dputl (CompactSumLF k l) (M.Sum f dy dw, cl) =
		let (dz, cl') = F.dputl l (dw, M.bool f cl (F.missing l))
		in (M.Sum (M.retype f) (L.dputl k dy) dz, cl')

data Sum k l = Sum k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l) => Bidirectional (Sum k l) where
	type L (Sum k l) = [M.SumAtom (L k) (L l)]
	type R (Sum k l) = [M.SumAtom (R k) (R l)]

instance (F.Lens k, F.Lens l) => F.Lens (Sum k l) where
	type C  (Sum k l) = Either (C k) (C l)
	missing (Sum k l) = Left (F.missing k)
	dputr   (Sum k l) = F.foldState (dputSum (F.dputr k) (F.dputr l) k l)
	dputl   (Sum k l) = F.foldState (dputSum (F.dputl k) (F.dputl l) k l)

dputSum dputk dputl k l dv cv = case (dv, cv) of
	(M.SwitchLL dx, Left  c) -> switchll $ dputk (dx, ck)
	(M.SwitchLR dz, Left  c) -> switchlr $ dputl (dz, cl)
	(M.SwitchRL dx, Right c) -> switchrl $ dputk (dx, ck)
	(M.SwitchRR dz, Right c) -> switchrr $ dputl (dz, cl)
	(M.StayL    dx, Left  c) -> stayl    $ dputk (dx, c )
	(M.StayR    dz, Right c) -> stayr    $ dputl (dz, c )
	(_, c) -> ([M.FailSum], c)
	where
	ck = F.missing k
	cl = F.missing l
	switchll (dy, c) = ([M.SwitchLL dy], Left  c)
	switchlr (dw, c) = ([M.SwitchLR dw], Right c)
	switchrl (dy, c) = ([M.SwitchRL dy], Left  c)
	switchrr (dw, c) = ([M.SwitchRR dw], Right c)
	stayl    (dy, c) = ([M.StayL    dy], Left  c)
	stayr    (dw, c) = ([M.StayR    dw], Right c)
