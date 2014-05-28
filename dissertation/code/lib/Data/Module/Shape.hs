{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Data.Module.Shape where

import Algebra.PartialOrd
import Data.Monoid
import Data.Module.Class
import Data.Container
import qualified Data.Set as S

instance Module (Sum Int) where
	type V (Sum Int) = Int
	apply (Sum di) i = Just (max 0 (di + i))
	-- Okay, doesn't quite obey the laws, what with overflow and all.
type instance ShapeModule Int = Sum Int
instance PartialOrd Int where leq = (<=)
instance ContainerType Int where
	type P Int = Int
	live i = S.fromAscList [0..i-1]

listToContainer :: [a] -> Container Int a
containerToList :: Container Int a -> [a]
listToContainer as = Container (length as) (as!!)
containerToList c  = [containedValues c i | i <- [0..currentShape c-1]]
