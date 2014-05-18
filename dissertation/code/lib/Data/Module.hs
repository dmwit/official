{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Data.Module
	( module Data.Default
	, module Data.Module.Class
	, module Data.Module.List
	, module Data.Module.Primitive
	, module Data.Module.Product
	, module Data.Module.Shape
	, module Data.Module.Sum
	, module Data.Monoid
	) where

import Data.Default
import Data.Module.Class
import Data.Module.List
import Data.Module.Primitive
import Data.Module.Product
import Data.Module.Shape
import Data.Module.Sum
import Data.Monoid hiding (Sum(..))
