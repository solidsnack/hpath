
module HPath.Hierarchy where

import Data.List
import System.FilePath

import HPath.Path




{-| Produce file paths to search for this Haskell name, accomodating JHC style
    paths as well as GHC style paths. The GHC style path comes first in order.
 -}
paths                       ::  [Char] -> Path -> [[Char]]
paths dir (Path mods m _)    =  reverse
  [ name i t | i <- inits mods | t <- tails mods ]
 where
  name i t = intercalate "/" ((dir:i) ++ [intercalate "." (t ++ [f])])
  f                          =  m ++ ".hs"




