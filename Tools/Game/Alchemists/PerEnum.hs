{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Tools.Game.Alchemists.PerEnum (
  PerEnum,
  get,
  set,
  new,
  build,
  combine,
  update
) where

class (Bounded e, Enum e) => PerEnum p e | p -> e where
  get :: e -> p a -> a
  set :: e -> a -> p a -> p a
  new :: p a

  build :: (e -> a) -> p a
  build f = foldr (\e p -> set e (f e) p) new [minBound .. maxBound]

  combine :: (a -> b -> c) -> p a -> p b -> p c
  combine f a b = build comb
    where comb e = f (get e a) (get e b)

  update :: (a -> a) -> e -> p a -> p a
  update f e p = set e (f (get e p)) p
