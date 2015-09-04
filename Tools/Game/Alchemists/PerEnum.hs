{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Tools.Game.Alchemists.PerEnum (
  PerEnum(..),
  update,
  keys
) where

import Prelude hiding (init)

class (Bounded e, Enum e) => PerEnum p e | p -> e where
  get :: e -> p a -> a
  set :: e -> a -> p a -> p a
  new :: p a

  build :: (e -> a) -> p a
  build f = foldr (\e p -> set e (f e) p) new [minBound .. maxBound]

  init :: a -> p a
  init = build . const

  combine :: (a -> b -> c) -> p a -> p b -> p c
  combine f a b = build comb
    where comb e = f (get e a) (get e b)

update :: (PerEnum p e) => (a -> a) -> e -> p a -> p a
update f e p = set e (f (get e p)) p

keys :: (PerEnum p e) => p a -> [e]
keys _ = [minBound .. maxBound]
