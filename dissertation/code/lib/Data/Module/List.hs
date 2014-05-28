{-# LANGUAGE TypeFamilies #-}
module Data.Module.List where

import Control.Monad
import Data.Default
import Data.List
import Data.Module.Class

-- in the Simple variant, the elements of the list tell which index the
-- corresponding element in the output list should come from, e.g.
-- applyPermutation (Simple is) [1..length is] = is
data Permutation = Simple [Integer] | Complex (Integer -> Integer -> Integer)

applyPermutation :: Permutation -> [a] -> [a]
applyPermutation (Complex f) xs = result where
	result = [genericIndex xs (f n i - 1) | i <- [1..n]]
	n      = genericLength xs
applyPermutation (Simple is) xs
	| length xs < length is = xs
	| otherwise = map (\i -> genericIndex xs (i-1)) is ++ drop (length is) xs

complexPermutation :: Permutation -> Integer -> Integer -> Integer
complexPermutation (Simple is) = \n i -> case () of
	_ | n < len   -> i
	  | i > len   -> i
	  | i > n     -> error
	  	 $ "asked a premutation for the origin of position "    ++ show i
	  	++ ", but the permutation is of a list of length only " ++ show n
	  | otherwise -> genericIndex is (i-1)
	where len = genericLength is
complexPermutation (Complex f) = f

simplePermutation :: Permutation -> Integer -> [Integer]
simplePermutation (Simple is) n = is
simplePermutation (Complex f) n = [f n i | i <- [1 .. n]]

instance Show Permutation where
	showsPrec d (Complex f) = showString "<fn>"
	showsPrec d (Simple ns)
		= showString "["
		. showString (intercalate ", " (zipWith arrow ns [1..]))
		. showString "]"
		where arrow n i = show n ++ "->" ++ show i

data ListAtom dX
	= FailList
	| Modify Integer dX
	| Insert Integer
	| Delete Integer
	| Rearrange Permutation
	deriving Show

split3 :: Integer -> [a] -> Maybe ([a], a, [a])
split3 i xs | i < 1 = Nothing
split3 i xs = case genericSplitAt (i-1) xs of
	(b, x:e) -> Just (b, x, e)
	_ -> Nothing

instance Module dX => PartialEdit (ListAtom dX) where
	type V_0 (ListAtom dX) = [V dX]
	apply_0 (Modify p dx) xs = do
		(b, x, e) <- split3 p xs
		x' <- apply dx x
		return (b ++ [x'] ++ e)
	apply_0 (Insert i) xs = return (xs ++ genericReplicate i def)
	apply_0 (Delete i) xs = do
		guard (0 <= i && i <= genericLength xs)
		return (zipWith const xs (genericDrop i xs))
	apply_0 (Rearrange perm) xs = return (applyPermutation perm xs)
	apply_0 _ _ = Nothing
