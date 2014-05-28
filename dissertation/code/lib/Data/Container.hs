{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Data.Container where

import Algebra.PartialOrd
import Data.Default
import Data.Module.Class
import Data.Set

type family ShapeModule shape

class ( V (ShapeModule shape) ~ shape
      , Module (ShapeModule shape)
      , PartialOrd shape
      , Ord (P shape)
      )
      => ContainerType shape where
	type P shape -- _p_ositions
	live :: shape -> Set (P shape) -- monotone

data Container shape element = Container
	{ currentShape    :: shape
	, containedValues :: P shape -> element -- only need be defined for "live" shapes
	}

instance (Default shape, Default element) => Default (Container shape element) where
	def = Container def (const def)

replace p e c = c { containedValues = \p' -> if p == p' then e else containedValues c p' }
