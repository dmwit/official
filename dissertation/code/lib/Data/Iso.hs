module Data.Iso where

data Iso a b = Iso (a -> b) (b -> a)
instance Show (Iso a b) where show (Iso f g) = "Iso <fn> <fn>"
