{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Module.Primitive where

import Data.Default
import Data.Module.Class
import Data.Monoid

newtype Unit x = Unit () deriving Monoid

instance Default x => Module (Unit x) where
	type V (Unit x) = x
	apply _ = Just

instance Default x => Module (First x) where
	type V (First x) = x
	apply (First Nothing  ) x = Just x
	apply (First (Just x')) x = Just x'
