{-# LANGUAGE TypeFamilies #-}
module Data.Lens.Edit.Primitive where

import Control.Arrow (first)
import Data.Lens.Bidirectional
import Data.Lens.Edit.Stateful (C) -- needed for GHC 7.2
import Data.Iso
import Data.Monoid
import qualified Data.Lens.Edit.Stateful  as F -- state_f_ul
import qualified Data.Lens.Edit.Stateless as L -- state_l_ess

data Id dX = Id deriving (Eq, Ord, Show, Read)
instance Bidirectional (Id dX) where
	type L (Id dX) = dX
	type R (Id dX) = dX

instance F.Lens (Id dX) where
	type C (Id dX) = ()
	missing = const ()
	dputr   = const id
	dputl   = const id

instance L.Lens (Id dX) where
	dputr = const id
	dputl = const id

data Compose k l = Compose k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l, R k ~ L l) => Bidirectional (Compose k l) where
	type L (Compose k l) = L k
	type R (Compose k l) = R l

instance (F.Lens k, F.Lens l, R k ~ L l) => F.Lens (Compose k l) where
	type C (Compose k l) = (F.C k, F.C l)
	missing  (Compose k l) = (F.missing k, F.missing l)
	dputr (Compose k l) (dx, (ck, cl)) =
		let (dy, ck') = F.dputr k (dx, ck)
		    (dz, cl') = F.dputr l (dy, cl)
		in (dz, (ck', cl'))
	dputl (Compose k l) (dz, (ck, cl)) =
		let (dy, cl') = F.dputl l (dz, cl)
		    (dx, ck') = F.dputl k (dy, ck)
		in (dx, (ck', cl'))

instance (L.Lens k, L.Lens l, R k ~ L l) => L.Lens (Compose k l) where
	dputr (Compose k l) = L.dputr l . L.dputr k
	dputl (Compose k l) = L.dputl k . L.dputl l

data ComposeFL k l = ComposeFL k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l, R k ~ L l) => Bidirectional (ComposeFL k l) where
	type L (ComposeFL k l) = L k
	type R (ComposeFL k l) = R l

instance (F.Lens k, L.Lens l, R k ~ L l) => F.Lens (ComposeFL k l) where
	type C (ComposeFL k l) = F.C k
	missing  (ComposeFL k l) = F.missing k
	dputr (ComposeFL k l) = first (L.dputr l) . F.dputr k
	dputl (ComposeFL k l) = F.dputl k . first (L.dputl l)

data ComposeLF k l = ComposeLF k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l, R k ~ L l) => Bidirectional (ComposeLF k l) where
	type L (ComposeLF k l) = L k
	type R (ComposeLF k l) = R l

instance (L.Lens k, F.Lens l, R k ~ L l) => F.Lens (ComposeLF k l) where
	type C (ComposeLF k l) = F.C l
	missing  (ComposeLF k l) = F.missing l
	dputr (ComposeLF k l) = F.dputr l . first (L.dputr k)
	dputl (ComposeLF k l) = first (L.dputl k) . F.dputl l

data Op l = Op l deriving (Eq, Ord, Show, Read)
unOp (Op l) = l
instance Bidirectional l => Bidirectional (Op l) where
	type L (Op l) = R l
	type R (Op l) = L l

instance F.Lens l => F.Lens (Op l) where
	type C (Op l) = F.C l
	missing = F.missing . unOp
	dputr   = F.dputl   . unOp
	dputl   = F.dputr   . unOp

instance L.Lens l => L.Lens (Op l) where
	dputr = L.dputl . unOp
	dputl = L.dputr . unOp

data Disconnect dX dY = Disconnect deriving (Eq, Ord, Show, Read)
instance Bidirectional (Disconnect dX dY) where
	type L (Disconnect dX dY) = dX
	type R (Disconnect dX dY) = dY

instance (Monoid dX, Monoid dY) => F.Lens (Disconnect dX dY) where
	type C (Disconnect dX dY) = ()
	missing = const ()
	dputr _ (_, c) = (mempty, c)
	dputl _ (_, c) = (mempty, c)

instance (Monoid dX, Monoid dY) => L.Lens (Disconnect dX dY) where
	dputr = const (const mempty)
	dputl = const (const mempty)

instance Bidirectional (Iso dX dY) where
	type L (Iso dX dY) = dX
	type R (Iso dX dY) = dY

instance F.Lens (Iso dX dY) where
	type C (Iso dX dY) = ()
	missing = const ()
	dputr (Iso f g) (dx, c) = (f dx, c)
	dputl (Iso f g) (dy, c) = (g dy, c)

instance L.Lens (Iso dX dY) where
	dputr (Iso f g) = f
	dputl (Iso f g) = g
