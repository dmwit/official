{-# LANGUAGE TypeFamilies #-}
module Data.Lens.Edit.Stateful where

import Data.Lens.Bidirectional
import Data.Monoid

class Bidirectional l => Lens l where
	type C l
	missing :: l -> C l
	dputr   :: l -> (L l, C l) -> (R l, C l)
	dputl   :: l -> (R l, C l) -> (L l, C l)

-- Morally, we have
-- foldMap :: Monoid b => (a -> State c b) -> ([a] -> State c b)
-- which does just what we want.  Unfortunately, this requires an
-- instance (Monad m, Monoid a) => Monoid (m a)
-- and an unhealthy amount of type munging to get in and out of State, curry
-- arguments, etc.  Since the instance above is most conveniently available
-- from the "reducers" package, which has a dependency redwood, and the
-- above-mentioned type-munging obfuscates the beautiful definition anyway, we
-- instead re-implement foldMap manually. It's not quite as beautiful
-- conceptually, but it makes for much easier reading.

foldState :: Monoid dY => (dX -> c -> (dY, c)) -> ([dX], c) -> (dY, c)
foldState f ([]  , c) = (mempty, c)
foldState f (e:es, c) = (mappend e1 e2, c'') where
	(e2, c' ) = foldState f (es, c)
	(e1, c'') = f e c'
