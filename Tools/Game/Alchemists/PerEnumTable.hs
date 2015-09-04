module Tools.Game.Alchemists.PerEnumTable (
  build,
  init,
  get,
  rowKeys,
  colKeys
) where

import Prelude hiding (init)
import qualified Tools.Game.Alchemists.PerEnum as PE

build :: (PE.PerEnum p1 c, PE.PerEnum p2 r) => (c -> r -> a) -> p1 (p2 a)
build f = PE.build (PE.build . f)

init :: (PE.PerEnum p1 c, PE.PerEnum p2 r) => a -> p1 (p2 a)
init = build . const . const

get :: (PE.PerEnum p1 c, PE.PerEnum p2 r) => c -> r -> p1 (p2 a) -> a
get c r = PE.get r . PE.get c 

rowKeys :: (PE.PerEnum p1 c, PE.PerEnum p2 r) => p1 (p2 a) -> [r]
rowKeys _ = [minBound .. maxBound]

colKeys :: (PE.PerEnum p1 c, PE.PerEnum p2 r) => p1 (p2 a) -> [c]
colKeys _ = [minBound .. maxBound]
