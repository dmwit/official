{-# LANGUAGE TypeFamilies #-}

module Data.Lens.Bidirectional where

class Bidirectional l where
	type L l
	type R l
