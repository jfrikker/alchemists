{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

{-|
  Module: Tools.Game.Alchemists.PerEnum

  Many of the functions in the Alchemists library involve generating a mapping from an enumeration to a value. For
  example, the assignment of ingredients to alchemicals is a mapping from ingredient (an enumeration) to an
  alchemical. Because of the small (and constant) number of keys, Data.Map is overkill. It makes more sense to create
  a data structure with an explicit slot for each value. For example:

  > data PerIngredient a = PI a a a a a a a a

  The PI constructor has 8 slots, one for each of the 8 ingredients. Mappings of this nature can implement the PerEnum
  class to gain the benefits of the functions defined here.
-}
module Tools.Game.Alchemists.PerEnum (
  PerEnum(..),
  update,
  keys
) where

import Prelude hiding (init)

-- |The PerEnum class
class (Bounded e, Enum e) => PerEnum p e | p -> e where
  -- |Extracts the value corresponding to a key.
  get :: e -> p a -> a

  -- |Returns a new mapping with the value for a key set to a new value.
  set :: e -> a -> p a -> p a

  -- |Constructs a new, empty mapping. It is expected that none of the values will be read; rather they will be
  -- replaced using 'set'. It is safe to set all the values to 'undefined'.
  new :: p a

  -- |Constructs a new mapping by applying a function to each enumerated value.
  build :: (e -> a) -> p a
  build f = foldr (\e p -> set e (f e) p) new [minBound .. maxBound]

  -- |Constructs a new mapping with every value set to a constant.
  init :: a -> p a
  init = build . const

  -- |Combines two mappings by applying a function to their values, producing a third mapping.
  combine :: (a -> b -> c) -> p a -> p b -> p c
  combine f a b = build comb
    where comb e = f (get e a) (get e b)

  {-# MINIMAL get, set, new #-}

-- |Applies a function to a single value in a mapping, and updates its value.
update :: (PerEnum p e) => (a -> a) -> e -> p a -> p a
update f e p = set e (f (get e p)) p

-- |Returns all the enumerated keys of a mapping.
keys :: (PerEnum p e) => p a -> [e]
keys _ = [minBound .. maxBound]
