{-# LANGUAGE TypeFamilies #-}
module Data.Lens.Edit.Stateless where

import Data.Lens.Bidirectional

class Bidirectional l => Lens l where
	dputr :: l -> L l -> R l
	dputl :: l -> R l -> L l
