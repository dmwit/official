module Data.Lens.Edit.String where

import Data.Lens.Edit.Container
import Data.Lens.Edit.Primitive
import Data.Lens.Edit.Product
import Data.Module.String

-- to shorten some type declarations
type Match' = NewDefaultMatch
type Edit'  = NewDefaultModule

copyEmpty :: String -> (Match, Id [Edit], Match)
copyEmpty s = (Match s, Id, Match s)

copyNonEmpty :: v -> String -> (Match' v, Id (Edit' v), Match' v)
copyNonEmpty v s = (NewDefaultMatch s, Id, NewDefaultMatch s)

skipEmpty :: String -> (Match, Disconnect [Edit] [Edit], Match)
skipEmpty s = (Match s, Disconnect, Match "")

skipNonEmpty :: v -> String -> (Match' v, Disconnect (Edit' v) [Edit], Match)
skipNonEmpty v s = (NewDefaultMatch s, Disconnect, Match "")

(#) :: (a, k, b) -> (c, l, d) -> ((a, c), CompactProduct k l, (b, d))
(#) (a, k, b) (c, l, d) = ((a, c), CompactProduct k l, (b, d))

op :: (a, l, b) -> (b, Op l, a)
op (a, l, b) = (b, Op l, a)

star :: (a, l, b) -> (Asterisk a, Map Int l, Asterisk b)
star (a, l, b) = (Asterisk a, Map l, Asterisk b)
