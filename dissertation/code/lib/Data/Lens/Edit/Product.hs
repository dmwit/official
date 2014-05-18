{-# LANGUAGE TypeFamilies #-}
module Data.Lens.Edit.Product where

import Control.Arrow
import Data.Lens.Bidirectional
import Data.Lens.Edit.Stateful (C) -- needed for GHC 7.2
import Data.Module.Product
import qualified Data.Lens.Edit.Stateful  as F -- state_f_ul
import qualified Data.Lens.Edit.Stateless as L -- state_l_ess

swizzleFF ((a, b), (c, d)) = ((a, c), (b, d))
data CompactProduct k l = CompactProduct k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l) => Bidirectional (CompactProduct k l) where
	type L (CompactProduct k l) = (L k, L l)
	type R (CompactProduct k l) = (R k, R l)

instance (F.Lens k, F.Lens l) => F.Lens (CompactProduct k l) where
	type C  (CompactProduct k l) = (F.C k, F.C l)
	missing (CompactProduct k l) = (F.missing k, F.missing l)
	dputr   (CompactProduct k l) = swizzleFF . (F.dputr k *** F.dputr l) . swizzleFF
	dputl   (CompactProduct k l) = swizzleFF . (F.dputl k *** F.dputl l) . swizzleFF

instance (L.Lens k, L.Lens l) => L.Lens (CompactProduct k l) where
	dputr (CompactProduct k l) = L.dputr k *** L.dputr l
	dputl (CompactProduct k l) = L.dputl k *** L.dputl l

swizzleFL ((a, b), c) = ((a, c), b)
data CompactProductFL k l = CompactProductFL k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l) => Bidirectional (CompactProductFL k l) where
	type L (CompactProductFL k l) = (L k, L l)
	type R (CompactProductFL k l) = (R k, R l)

instance (F.Lens k, L.Lens l) => F.Lens (CompactProductFL k l) where
	type C  (CompactProductFL k l) = F.C k
	missing (CompactProductFL k l) = F.missing k
	dputr   (CompactProductFL k l) = swizzleFL . (F.dputr k *** L.dputr l) . swizzleFL
	dputl   (CompactProductFL k l) = swizzleFL . (F.dputl k *** L.dputl l) . swizzleFL

swizzleLF   ((a, b), c) = (a, (b, c))
unswizzleLF (a, (b, c)) = ((a, b), c)
data CompactProductLF k l = CompactProductLF k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l) => Bidirectional (CompactProductLF k l) where
	type L (CompactProductLF k l) = (L k, L l)
	type R (CompactProductLF k l) = (R k, R l)

instance (L.Lens k, F.Lens l) => F.Lens (CompactProductLF k l) where
	type C  (CompactProductLF k l) = F.C l
	missing (CompactProductLF k l) = F.missing l
	dputr   (CompactProductLF k l) = unswizzleLF . (L.dputr k *** F.dputr l) . swizzleLF
	dputl   (CompactProductLF k l) = unswizzleLF . (L.dputl k *** F.dputl l) . swizzleLF

data Product k l = Product k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l) => Bidirectional (Product k l) where
	type L (Product k l) = [ProductAtom (L k) (L l)]
	type R (Product k l) = [ProductAtom (R k) (R l)]

instance (F.Lens k, F.Lens l) => F.Lens (Product k l) where
	type C  (Product k l) = (F.C k, F.C l)
	missing (Product k l) = (F.missing k, F.missing l)
	dputr   (Product k l) = F.foldState (dputProductF (F.dputr k) (F.dputr l))
	dputl   (Product k l) = F.foldState (dputProductF (F.dputl k) (F.dputl l))

dputProductF dputk dputl (Left  dx) (ck, cl) = let (dz, ck') = dputk (dx, ck) in ([Left  dz], (ck', cl))
dputProductF dputk dputl (Right dy) (ck, cl) = let (dw, cl') = dputl (dy, cl) in ([Right dw], (ck, cl'))

instance (L.Lens k, L.Lens l) => L.Lens (Product k l) where
	dputr (Product k l) = map (either (Left . L.dputr k) (Right . L.dputr l))
	dputl (Product k l) = map (either (Left . L.dputl k) (Right . L.dputl l))

data ProductFL k l = ProductFL k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l) => Bidirectional (ProductFL k l) where
	type L (ProductFL k l) = [ProductAtom (L k) (L l)]
	type R (ProductFL k l) = [ProductAtom (R k) (R l)]

instance (F.Lens k, L.Lens l) => F.Lens (ProductFL k l) where
	type C  (ProductFL k l) = F.C k
	missing (ProductFL k l) = F.missing k
	dputr   (ProductFL k l) = F.foldState (dputProductFL (F.dputr k) (L.dputr l))
	dputl   (ProductFL k l) = F.foldState (dputProductFL (F.dputl k) (L.dputl l))

dputProductFL dputk dputl (Left  dx) ck = let (dz, ck') = dputk (dx, ck) in ([Left dz], ck')
dputProductFL dputk dputl (Right dy) ck = ([Right (dputl dy)], ck)

data ProductLF k l = ProductLF k l deriving (Eq, Ord, Show, Read)
instance (Bidirectional k, Bidirectional l) => Bidirectional (ProductLF k l) where
	type L (ProductLF k l) = [ProductAtom (L k) (L l)]
	type R (ProductLF k l) = [ProductAtom (R k) (R l)]

instance (L.Lens k, F.Lens l) => F.Lens (ProductLF k l) where
	type C  (ProductLF k l) = F.C l
	missing (ProductLF k l) = F.missing l
	dputr   (ProductLF k l) = F.foldState (dputProductLF (L.dputr k) (F.dputr l))
	dputl   (ProductLF k l) = F.foldState (dputProductLF (L.dputl k) (F.dputl l))

dputProductLF dputk dputl (Left  dx) cl = ([Left (dputk dx)], cl)
dputProductLF dputk dputl (Right dy) cl = let (dw, cl') = dputl (dy, cl) in ([Right dw], cl')
