
module HPath.Hierarchy where

import Data.List

import HPath.Path




{-| Produce file paths to search for this Haskell name, accomodating JHC style
    paths as well as GHC style paths. The GHC style path comes first in order.
 -}
paths                       ::  Path -> [[Char]]
paths (Path mods m _)    =  reverse
  [ name i t | i <- inits mods | t <- tails mods ]
 where
  name i t = intercalate "/" (i ++ [intercalate "." (t ++ [f])])
  f                          =  m ++ ".hs"




