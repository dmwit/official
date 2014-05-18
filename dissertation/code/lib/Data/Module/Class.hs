{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Module.Class where

import Data.Default
import Data.Maybe
import Data.Monoid

class (Default (V dX), Monoid dX) => Module dX where
	type V dX
	apply :: dX -> V dX -> Maybe (V dX)

applyDef :: Module dX => dX -> Maybe (V dX)
applyDef dx = apply dx def

applyTotal :: Module dX => dX -> V dX -> V dX
applyTotal dx x = fromJust (apply dx x)

applyDefTotal :: Module dX => dX -> V dX
applyDefTotal dx = applyTotal dx def

class Default (V_0 dX) => PartialEdit dX where
	type V_0 dX
	apply_0 :: dX -> V_0 dX -> Maybe (V_0 dX)

instance PartialEdit dX => Module [dX] where
	type V [dX] = V_0 dX
	apply []       v = Just v
	apply (dx:dxs) v = apply dxs v >>= apply_0 dx
