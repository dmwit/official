{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}
module Data.Module.Product where

import Control.Monad
import Data.Module.Class

instance (Module dX, Module dY) => Module (dX, dY) where
	type V (dX, dY) = (V dX, V dY)
	apply (dx, dy) (x, y) = liftM2 (,) (apply dx x) (apply dy y)

type ProductAtom = Either

instance (Module dX, Module dY) => PartialEdit (ProductAtom dX dY) where
	type V_0 (ProductAtom dX dY) = (V dX, V dY)
	apply_0 (Left  dx) (x, y) = liftM2 (,) (apply dx x) (return   y)
	apply_0 (Right dy) (x, y) = liftM2 (,) (return   x) (apply dy y)
