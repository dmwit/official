{-# LANGUAGE TypeFamilies #-}
module Data.Module.Container where

import Control.Applicative
import Data.Container
import Data.Default
import Data.Module.Class
import qualified Data.Set as S

data ContainerAtom shape dX
	= FailContainer
	| Modify (P shape) dX
	| Insert (ShapeModule shape) -- a non-decreasing edit to the shape
	| Delete (ShapeModule shape) -- a non-increasing edit to the shape
	-- a shape edit which doesn't change the number of positions, and a
	-- function translating positions in the new structure to their
	-- corresponding position in the old structure
	| Rearrange (ShapeModule shape) (shape -> P shape -> P shape)

instance (Show (P shape), Show (ShapeModule shape), Show dX)
	=> Show (ContainerAtom shape dX) where
	showsPrec d FailContainer = showString "FailContainer"
	showsPrec d (Modify pos dx)
		= showParen (d > 10)
		$ showString "Modify "
		. showsPrec 11 pos
		. showString " "
		. showsPrec 11 dx
	showsPrec d (Insert ds)
		= showParen (d > 10)
		$ showString "Insert "
		. showsPrec 11 ds
	showsPrec d (Delete ds)
		= showParen (d > 10)
		$ showString "Delete "
		. showsPrec 11 ds
	showsPrec d (Rearrange ds f)
		= showParen (d > 10)
		$ showString "Rearrange "
		. showsPrec 11 ds
		. showString " <fn>"

instance (ContainerType shape, Module dX) => PartialEdit (ContainerAtom shape dX) where
	type V_0 (ContainerAtom shape dX) = Container shape (V dX)
	apply_0 (FailContainer ) _ = Nothing
	apply_0 (Modify    p dx) c = (\x -> replace p x   c) <$> apply dx (containedValues c p)
	apply_0 (Insert    ds  ) c = (\s -> expand  s def c) <$> apply ds (currentShape    c  )
	apply_0 (Delete    ds  ) c = (\s -> setShape  s   c) <$> apply ds (currentShape    c  )
	apply_0 (Rearrange ds f) c = (\s -> reorder f s   c) <$> apply ds (currentShape    c  )

expand shape' x (Container shape values) = Container shape' $ \p ->
	if S.member p (live shape)
	then values p
	else x

setShape shape c = c { currentShape = shape }
reorder f shape' (Container shape values) = Container shape' (values . f shape)
