{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Module.Sum where

import Data.Default
import Data.Module.Class
import Data.Monoid hiding (Sum(..))

instance Default x => Default (Either x y) where
	def = Left def

data Tag = L | R deriving (Eq, Ord, Bounded, Enum, Show, Read)
newtype Retag x y = Retag (Maybe (Endo Tag)) deriving Monoid

retype (Retag f) = Retag f
bool (Retag Nothing) nothing just = nothing
bool _               nothing just = just

instance Eq (Retag x y) where
	Retag Nothing  == Retag Nothing   = True
	Retag (Just f) == Retag (Just f') = map (appEndo f) [L, R] == map (appEndo f') [L, R]
	_ == _ = False

instance (Default x, Default y) => Module (Retag x y) where
	type V (Retag x y) = Either x y
	apply (Retag Nothing) v = Just v
	apply (Retag (Just (Endo f))) v = Just $ case v of
		Left  x -> redef (f L)
		Right y -> redef (f R)
		where
		redef L = Left  def
		redef R = Right def

data Sum dX dY = Sum (Retag (V dX) (V dY)) dX dY
instance (Monoid dX, Monoid dY) => Monoid (Sum dX dY) where
	mempty = Sum mempty mempty mempty
	mappend (Sum f dx dy) (Sum f' dx' dy') =
		Sum (mappend f f') (annihilate dx dx') (annihilate dy dy') where
		annihilate :: Monoid d => d -> d -> d
		annihilate d d' = bool f d (mappend d d')

instance (Module dX, Module dY) => Module (Sum dX dY) where
	type V (Sum dX dY) = Either (V dX) (V dY)
	apply (Sum f dx dy) v = apply f v >>= either
		(fmap Left  . apply dx)
		(fmap Right . apply dy)

data SumAtom dX dY
	= FailSum
	| SwitchLL dX
	| SwitchLR dY
	| SwitchRL dX
	| SwitchRR dY
	| StayL dX
	| StayR dY
	deriving (Eq, Ord, Show, Read)

instance (Module dX, Module dY) => PartialEdit (SumAtom dX dY) where
	type V_0 (SumAtom dX dY) = Either (V dX) (V dY)
	apply_0 (SwitchLL dx) (Left  x) = fmap Left  (apply dx x  )
	apply_0 (SwitchLR dy) (Left  x) = fmap Right (apply dy def)
	apply_0 (SwitchRL dx) (Right y) = fmap Left  (apply dx def)
	apply_0 (SwitchRR dy) (Right y) = fmap Right (apply dy y  )
	apply_0 (StayL dx) (Left  x) = fmap Left  (apply dx x)
	apply_0 (StayR dy) (Right y) = fmap Right (apply dy y)
	apply_0 _ _ = Nothing
