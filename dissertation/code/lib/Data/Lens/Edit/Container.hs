{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeOperators #-}
module Data.Lens.Edit.Container where

import Data.Container
import Data.Default
import Data.Iso
import Data.Lens.Bidirectional
import Data.Lens.Edit.Stateful (C) -- needed for GHC 7.2
import Data.Module.Class
import Data.Module.Container
import qualified Data.Lens.Edit.Stateful  as F -- state_f_ul
import qualified Data.Lens.Edit.Stateless as L -- state_l_ess
import qualified Data.Set                 as S

data Map shape l = Map l deriving (Eq, Ord, Show, Read)

instance Bidirectional l => Bidirectional (Map shape l) where
	type L (Map shape l) = [ContainerAtom shape (L l)]
	type R (Map shape l) = [ContainerAtom shape (R l)]

instance (ContainerType shape, F.Lens l) => F.Lens (Map shape l) where
	type C (Map shape l) = Container shape (F.C l)
	missing (Map l) = Container def (const (F.missing l))
	dputr (Map l) = F.foldState (dputMapF F.dputr l)
	dputl (Map l) = F.foldState (dputMapF F.dputl l)

instance (ContainerType shape, L.Lens l) => L.Lens (Map shape l) where
	dputr (Map l) = map (dputMapL L.dputr l)
	dputl (Map l) = map (dputMapL L.dputl l)

dputMapF dput l FailContainer c = ([FailContainer], c)
dputMapF dput l (Modify p dx) c
	| S.member p (live (currentShape c)) = ([Modify p dy], replace p c' c)
	| otherwise = ([FailContainer], c)
	where (dy, c') = dput l (dx, containedValues c p)
dputMapF dput l (Insert ds) c = case apply ds (currentShape c) of
	Nothing -> ([FailContainer], c)
	Just s  -> ([Insert ds], expand s (F.missing l) c)
dputMapF dput l (Delete ds) c = case apply ds (currentShape c) of
	Nothing -> ([FailContainer], c)
	Just s  -> ([Delete ds], setShape s c)
dputMapF dput l (Rearrange ds f) c = case apply ds (currentShape c) of
	Nothing -> ([FailContainer], c)
	Just s  -> ([Rearrange ds f], reorder f s c)

dputMapL dput l (FailContainer ) = FailContainer
dputMapL dput l (Modify    p dx) = Modify p (dput l dx)
dputMapL dput l (Insert    ds  ) = Insert ds
dputMapL dput l (Delete    ds  ) = Delete ds
dputMapL dput l (Rearrange ds f) = Rearrange ds f
