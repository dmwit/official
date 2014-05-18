{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies #-}
module Data.Module.String where

import Control.Arrow
import Control.Monad
import Data.Algorithm.Diff
import Data.Default
import Data.List
import Data.Maybe
import Data.Module.Class
import Data.Module.Container (ContainerAtom)
import Data.Module.Product
import Data.Module.Shape
import Data.Monoid
import Text.Regex.PCRE
import qualified Data.Map as M
import qualified Data.Module.Container as C

class Module (M m) => StringModule m where
	type M m
	valid  :: m -> String -- returns a regex telling what strings are valid
	parse  :: m -> String -> Maybe (V (M m))
	pprint :: m -> V (M m) -> String
	edit   :: m -> V (M m) -> [Edit] -> M m

data Edit = Insert Int String | Delete Int Int deriving (Show, Read)

instance PartialEdit Edit where
	type V_0 Edit = String
	apply_0 (Insert n s') s = Just (take n s ++ s' ++ drop n s)
	apply_0 (Delete n n') s = Just (take n s ++ drop n' s)

newtype Match = Match String deriving (Eq, Ord, Show, Read)
maybeMatch re s = guard (s =~ re == (0 :: Int, length s)) >> return s
instance StringModule Match where
	type M Match = [Edit]
	valid (Match re) = re
	parse  = maybeMatch . valid
	pprint = const id
	edit   = const (const id)

instance (StringModule dX, StringModule dY) => StringModule (dX, dY) where
	type M (dX, dY) = (M dX, M dY)
	valid (mx, my) = valid mx ++ valid my
	parse (mx, my) s = do
		let (m , n ) =        s =~ valid mx
		    (m', n') = drop n s =~ valid my
		guard ([m, m', n + n'] == [0, 0, length s])
		vx <- parse mx (take n s)
		vy <- parse my (drop n s)
		return (vx, vy)
	pprint (mx, my) (vx, vy) = pprint mx vx ++ pprint my vy
	edit = editProd

editProd (mx, my) (vx, vy) es = edit mx vx *** edit my vy $ case splitMap of
	Nothing -> (replace sx sx', replace sy sy')
	Just ms -> (exs, eys)
	where
	die = error "The impossible happened! A [Edit] didn't successfully apply to a String in editProd."
	sx  = pprint mx vx
	sy  = pprint my vy
	sn  = fromMaybe die (apply es (sx ++ sy))
	oldSplit     = length sx
	newSplit     = snd (sn =~ valid mx :: (Int, Int))
	(sx', sy')   = splitAt newSplit sn
	easySplits   = trackEdits [oldSplit] es
	splitMap     = M.lookup newSplit easySplits
	-- this is dangerous, only use the variables it binds when splitMap is definitely a Just!
	(exs, eys)   = splitEdits (fromJust splitMap) oldSplit es
	replace s s' = [Insert 0 s', Delete 0 (length s)]

trackEdit :: Edit -> (Int, M.Map Int (M.Map Int Int)) -> (Int, M.Map Int (M.Map Int Int))
trackEdit (Insert n s) (i, m) = (i + 1, m') where
	(smaller, larger') = M.split n m
	len    = length s
	larger = M.mapKeysMonotonic (+ len) larger'
	exact  = case M.lookup n m of
		Nothing -> M.empty
		Just im -> M.fromList [(n + i', M.insert i i' im) | i' <- [0 .. len]]
	m'     = smaller `M.union` larger `M.union` exact
trackEdit (Delete n n') (i, m)
	| n >  n' = trackEdit (Delete n' n) (i, m)
	| n == n' = (i + 1, m)
	| n <  n' = (i + 1, m') where
	(smaller, notSmaller) = M.split (n +1) m
	(deleted, larger')    = M.split (n'-1) notSmaller
	len    = n' - n
	larger = M.mapKeysMonotonic (subtract len) larger'
	m'     = smaller `M.union` larger
	-- NOTE: this arbitrarily prefers the edits on the left-hand side of the
	-- deletion boundary over the edits on the right-hand side of the deletion
	-- boundary when a chunk boundary happens to fall on a position that
	-- matches the deletion boundary

-- input: the locations of chunk boundaries, plus some edits
-- returns:
--   The inner Map Int Int tells, for each index into the edits, whether to split the resulting edit and where.
--   The outer Map tells, for each position, if a chunk boundary ends up landing there, how to split the edits.
trackEdits :: [Int] -> [Edit] -> M.Map Int (M.Map Int Int)
trackEdits boundaries = snd . foldr trackEdit (0, M.fromList (zip boundaries (repeat M.empty)))

splitEdits :: M.Map Int Int -> Int -> [Edit] -> ([Edit], [Edit])
splitEdits m split = snd . foldr splitEdit ((0, split), ([], [])) where
	splitEdit :: Edit -> ((Int, Int), ([Edit], [Edit])) -> ((Int, Int), ([Edit], [Edit]))
	splitEdit e@(Delete n n') ((i, split), (els, ers))
		| n > n' = splitEdit (Delete n' n) ((i, split), (els, ers))
		| n' <= split = ((i+1, split-n'+n), (e:els, ers))
		| n  >= split = ((i+1, split), (els, Delete (n-split) (n'-split) : ers))
		| otherwise   = error "The impossible happened! A deletion crossed a chunk boundary."
		--  | otherwise   = ((i+1, n), (Delete n split : els, Delete 0 (n'-split) : ers))
	splitEdit e@(Insert n s) ((i, split), (els, ers)) = case M.lookup i m of
		Nothing
			| n < split -> ((i+1, split+length s), (e:els, ers))
			| n > split -> ((i+1, split), (els, Insert (n-split) s : ers))
			| otherwise -> error "The impossible happened! An insertion crossed a chunk boundary without being in the splitting map."
		Just n' -> ((i+1, split+n'), (consInsert n (take n' s) els, consInsert 0 (drop n' s) ers))
	consInsert n "" es = es
	consInsert n s  es = Insert n s : es

data Asterisk m = Asterisk m
instance StringModule m => StringModule (Asterisk m) where
	type M (Asterisk m) = [ContainerAtom Int (M m)]
	valid  (Asterisk m) = "(" ++ valid m ++ ")*"
	pprint (Asterisk m) = containerToList >=> pprint m
	edit   (Asterisk m) = editList m . containerToList
	parse  (Asterisk m) = liftM listToContainer . parseList m

parseList m = splitList (valid m) >=> mapM (parse m)
splitList re "" = return []
splitList re s  = do
	-- pattern match failure means the regex didn't match at the beginning,
	-- and results in a failed parse overall
	("", sMatch, rest) <- return (s =~ re)
	liftM (sMatch:) (splitList re rest)

-- Heuristic: if we can track where *all* the old splits went, then go ahead
-- and do that. Nice! Otherwise, use diff to compare the entire old list with
-- the entire new list.
--
-- There's a lot of room for improvement: we could take the regions between
-- successfully tracked splits and the regions with unsuccessfully tracked
-- splits and run the diff only on the unsuccessful regions, for example.
-- Additionally, note that when we are tracking splits, we assume that no
-- splits are inserted or deleted. A more sophisticated heuristic might try to
-- relax this assumption somehow (though the only way I could think of to relax
-- it would result in an exponential-time algorithm).

editList m vs es = fromMaybe diffy exact where
	exact = editListExact m vs oldss newss es 0
	diffy = editListDiff  m    oldss newss
	oldss = map (pprint m) vs
	newss = fromJust (splitList (valid m) =<< apply es (concat oldss)) :: [String]

editListExact m (v:vs) (olds:oldss) (news:newss) es i = case splitMap of
	Nothing -> Nothing
	Just ms -> liftM (modifyHere++) (editListExact m vs oldss newss erest (i+1))
	where
	oldSplit     = length olds
	newSplit     = length news
	easySplits   = trackEdits [oldSplit] es
	splitMap     = M.lookup newSplit easySplits
	-- this is dangerous, only use the variables it binds when splitMap is definitely a Just!
	(e, erest)   = splitEdits (fromJust splitMap) oldSplit es
	modifyHere   = [C.Modify i (edit m v e) | not (null e)]
editListExact m [] [] [] [] i = Just []
editListExact _ _  _  _  _  _ = Nothing

editListDiff m oldss newss = result where
	diff = getDiff oldss newss
	tags = map fst diff
	count place        = length . filter (==place) $ tags
	(countS, countF)   = (count S, count F)
	match place B      = [True]
	match place place' = [False | place == place']
	reordered place    = map fst . uncurry (++) . partition snd . zip [0..] $ (tags >>= match place)
	needsReorder place = any (==B) (dropWhile (place /=) tags)
	create s = edit m def [Insert 0 s, Delete 0 . length . pprint m $ def]
	result
		=  [C.Rearrange (Sum 0) (\_ i -> fromJust (findIndex (==i) (reordered S))) | needsReorder S]
		++ zipWith C.Modify [count B .. count B + count S - 1] [create news | (S, news) <- diff]
		++ [C.Insert . Sum          $ countS | countS > 0]
		++ [C.Delete . Sum . negate $ countF | countF > 0]
		++ [C.Rearrange (Sum 0) (\_ i -> reordered F !! i) | needsReorder F]

-- a few utilities for defining string modules whose default value is not ""
newtype NewDefault v = NewDefault String deriving (Eq, Ord)
instance Show (NewDefault v) where show (NewDefault s) = show s
instance Read (NewDefault v) where readsPrec n s = map (first NewDefault) (readsPrec n s)
instance (Default v, Show v) => Default (NewDefault v) where def = NewDefault (show (def :: v))
newtype NewDefaultModule v = NewDefaultModule [Edit] deriving (Show, Read, Monoid)
instance (Default v, Show v) => Module (NewDefaultModule v) where
	type V (NewDefaultModule v) = NewDefault v
	apply (NewDefaultModule es) (NewDefault v) = liftM NewDefault (apply es v)
newtype NewDefaultMatch v = NewDefaultMatch String deriving (Eq, Ord, Show, Read)

instance (Default v, Show v) => StringModule (NewDefaultMatch v) where
	type M (NewDefaultMatch v) = NewDefaultModule v
	valid (NewDefaultMatch re) = re
	parse  m                   = liftM NewDefault . maybeMatch (valid m)
	pprint m (NewDefault s)    = s
	edit                       = const (const NewDefaultModule)
