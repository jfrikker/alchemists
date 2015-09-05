{-|
  Module: Tools.Game.Alchemists.PerEnumTable

  A 'PerEnumTable' is a table made from two nested 'PerEnum's. For example:

  > PerIngredient (PerAlchemical a)

  This module defines functions that operate on this common structure. Note that, perhaps surprisingly, the outer
  PerEnum represents the columns, while the inner PerEnum represents the rows. This simply reflects the fact that many
  modules in this package prefer to deal with tables in this format.
-}
module Tools.Game.Alchemists.PerEnumTable (
  build,
  init,
  get,
  rowKeys,
  colKeys,
  tmap
) where

import Prelude hiding (init)
import qualified Tools.Game.Alchemists.PerEnum as PE

-- |Constructs a new table by applying a function to each cell.
build :: (PE.PerEnum p1 c, PE.PerEnum p2 r) => (c -> r -> a) -> p1 (p2 a)
build f = PE.build (PE.build . f)

-- |Constructs a new table with every cell set to a constant.
init :: (PE.PerEnum p1 c, PE.PerEnum p2 r) => a -> p1 (p2 a)
init = build . const . const

-- |Extracts the value stored in a cell.
get :: (PE.PerEnum p1 c, PE.PerEnum p2 r) => c -> r -> p1 (p2 a) -> a
get c r = PE.get r . PE.get c 

-- |The enumerated row keys for a table.
rowKeys :: (PE.PerEnum p1 c, PE.PerEnum p2 r) => p1 (p2 a) -> [r]
rowKeys _ = [minBound .. maxBound]

-- |The enumerated column keys for a table.
colKeys :: (PE.PerEnum p1 c, PE.PerEnum p2 r) => p1 (p2 a) -> [c]
colKeys _ = [minBound .. maxBound]

-- |Similar to fmap, but it works on tables.
tmap :: (PE.PerEnum p1 c, PE.PerEnum p2 r, Functor p1, Functor p2) => (a -> b) -> p1 (p2 a) -> p1 (p2 b)
tmap f = fmap (fmap f)
