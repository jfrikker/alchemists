{-# LANGUAGE MultiParamTypeClasses #-}

module Tools.Game.Alchemists.Reagents (
  Potion(..),
  Alchemical(..),
  Ingredient(..),
  Sign(..),
  PerIngredient,
  PerAlchemical,
  AlchemicalAssignment,
  AlchemicalConstraint,
  potionColor,
  potionSign,
  alchemicalProduct,
  allAssignments,
  createsPotion,
  alchemicalProbs,
  potionProb,
  signProb
) where

import qualified Tools.Game.Alchemists.PerEnum as PE
import qualified Data.List as L
import qualified Data.Ratio as R
import Data.Ratio ((%))

data Potion = BLUE_PLUS | BLUE_MINUS | RED_PLUS | RED_MINUS | GREEN_PLUS | GREEN_MINUS | NEUTRAL deriving (Show, Eq)

data Color = BLUE | RED | GREEN deriving (Show)

data Sign = PLUS | MINUS | NO_SIGN deriving (Show, Eq)

data Alchemical = AL_1 | AL_2 | AL_3 | AL_4 | AL_5 | AL_6 | AL_7 | AL_8 deriving (Show, Eq, Enum, Bounded)

data Ingredient = MUSHROOM | SPROUT | TOAD | FOOT | FLOWER | ROOT | SCORPION | FEATHER deriving (Show, Eq, Enum, Bounded)

potionColor :: Potion -> Color
potionColor BLUE_PLUS = BLUE
potionColor BLUE_MINUS = BLUE
potionColor RED_PLUS = RED
potionColor RED_MINUS = RED
potionColor GREEN_PLUS = GREEN
potionColor GREEN_MINUS = GREEN

potionSign :: Potion -> Sign
potionSign BLUE_PLUS = PLUS
potionSign BLUE_MINUS = MINUS
potionSign RED_PLUS = PLUS
potionSign RED_MINUS = MINUS
potionSign GREEN_PLUS = PLUS
potionSign GREEN_MINUS = MINUS
potionSign NEUTRAL = NO_SIGN

alchemicalProduct :: Alchemical -> Alchemical -> Potion
alchemicalProduct AL_1 AL_2 = NEUTRAL
alchemicalProduct AL_1 AL_3 = BLUE_MINUS
alchemicalProduct AL_1 AL_4 = GREEN_PLUS
alchemicalProduct AL_1 AL_5 = RED_MINUS
alchemicalProduct AL_1 AL_6 = BLUE_MINUS
alchemicalProduct AL_1 AL_7 = RED_MINUS
alchemicalProduct AL_1 AL_8 = GREEN_PLUS
alchemicalProduct AL_2 AL_3 = GREEN_MINUS
alchemicalProduct AL_2 AL_4 = BLUE_PLUS
alchemicalProduct AL_2 AL_5 = BLUE_PLUS
alchemicalProduct AL_2 AL_6 = RED_PLUS
alchemicalProduct AL_2 AL_7 = GREEN_MINUS
alchemicalProduct AL_2 AL_8 = RED_PLUS
alchemicalProduct AL_3 AL_4 = NEUTRAL
alchemicalProduct AL_3 AL_5 = GREEN_MINUS
alchemicalProduct AL_3 AL_6 = RED_PLUS
alchemicalProduct AL_3 AL_7 = BLUE_MINUS
alchemicalProduct AL_3 AL_8 = RED_PLUS
alchemicalProduct AL_4 AL_5 = RED_MINUS
alchemicalProduct AL_4 AL_6 = GREEN_PLUS
alchemicalProduct AL_4 AL_7 = RED_MINUS
alchemicalProduct AL_4 AL_8 = BLUE_PLUS
alchemicalProduct AL_5 AL_6 = NEUTRAL
alchemicalProduct AL_5 AL_7 = GREEN_MINUS
alchemicalProduct AL_5 AL_8 = BLUE_PLUS
alchemicalProduct AL_6 AL_7 = BLUE_MINUS
alchemicalProduct AL_6 AL_8 = GREEN_PLUS
alchemicalProduct AL_7 AL_8 = NEUTRAL
alchemicalProduct x y | x == y = error "Can't combine an alchemical with itself."
                      | otherwise = alchemicalProduct y x

data PerIngredient a = PI a a a a a a a a deriving (Show)

instance PE.PerEnum PerIngredient Ingredient where
  get MUSHROOM (PI x _ _ _ _ _ _ _) = x
  get SPROUT (PI _ x _ _ _ _ _ _) = x
  get TOAD (PI _ _ x _ _ _ _ _) = x
  get FOOT (PI _ _ _ x _ _ _ _) = x
  get FLOWER (PI _ _ _ _ x _ _ _) = x
  get ROOT (PI _ _ _ _ _ x _ _) = x
  get SCORPION (PI _ _ _ _ _ _ x _) = x
  get FEATHER (PI _ _ _ _ _ _ _ x) = x

  set MUSHROOM val (PI _ b c d e f g h) = PI val b c d e f g h
  set SPROUT val (PI a _ c d e f g h) = PI a val c d e f g h
  set TOAD val (PI a b _ d e f g h) = PI a b val d e f g h
  set FOOT val (PI a b c _ e f g h) = PI a b c val e f g h
  set FLOWER val (PI a b c d _ f g h) = PI a b c d val f g h
  set ROOT val (PI a b c d e _ g h) = PI a b c d e val g h
  set SCORPION val (PI a b c d e f _ h) = PI a b c d e f val h
  set FEATHER val (PI a b c d e f g _) = PI a b c d e f g val

  new = PI u u u u u u u u
    where u = undefined

type AlchemicalAssignment = PerIngredient Alchemical

data PerAlchemical a = PA a a a a a a a a deriving (Show)

instance PE.PerEnum PerAlchemical Alchemical where
  get AL_1 (PA x _ _ _ _ _ _ _) = x
  get AL_2 (PA _ x _ _ _ _ _ _) = x
  get AL_3 (PA _ _ x _ _ _ _ _) = x
  get AL_4 (PA _ _ _ x _ _ _ _) = x
  get AL_5 (PA _ _ _ _ x _ _ _) = x
  get AL_6 (PA _ _ _ _ _ x _ _) = x
  get AL_7 (PA _ _ _ _ _ _ x _) = x
  get AL_8 (PA _ _ _ _ _ _ _ x) = x

  set AL_1 val (PA _ b c d e f g h) = PA val b c d e f g h
  set AL_2 val (PA a _ c d e f g h) = PA a val c d e f g h
  set AL_3 val (PA a b _ d e f g h) = PA a b val d e f g h
  set AL_4 val (PA a b c _ e f g h) = PA a b c val e f g h
  set AL_5 val (PA a b c d _ f g h) = PA a b c d val f g h
  set AL_6 val (PA a b c d e _ g h) = PA a b c d e val g h
  set AL_7 val (PA a b c d e f _ h) = PA a b c d e f val h
  set AL_8 val (PA a b c d e f g _) = PA a b c d e f g val

  new = PA u u u u u u u u
    where u = undefined

type AlchemicalConstraint = AlchemicalAssignment -> Bool

ingredientProduct :: Ingredient -> Ingredient -> AlchemicalAssignment -> Potion
ingredientProduct i1 i2 assignment = alchemicalProduct (PE.get i1 assignment) (PE.get i2 assignment)

allAssignments :: [AlchemicalAssignment]
allAssignments = map assignmentFromList $ L.permutations [AL_1 .. AL_8]

assignmentFromList :: [Alchemical] -> AlchemicalAssignment
assignmentFromList [al1, al2, al3, al4, al5, al6, al7, al8] = PI al1 al2 al3 al4 al5 al6 al7 al8

createsPotion :: Ingredient -> Ingredient -> Potion -> AlchemicalConstraint
createsPotion i1 i2 potion assignment = potion == ingredientProduct i1 i2 assignment

alchemicalProbs :: Ingredient -> [AlchemicalAssignment] -> PerAlchemical R.Rational
alchemicalProbs i assignments = foldr inc (PE.init (0%1)) assignments
  where inc = PE.update (+ 1%len) . PE.get i
        len = fromIntegral $ length assignments

productProb :: (Potion -> Bool) -> Ingredient -> Ingredient -> [AlchemicalAssignment] -> R.Rational
productProb f i1 i2 assignments = foldr inc (0%1) assignments / fromIntegral (length assignments)
  where inc assignment count | f $ ingredientProduct i1 i2 assignment = count + (1%1)
                             | otherwise = count

potionProb :: Potion -> Ingredient -> Ingredient -> [AlchemicalAssignment] -> R.Rational
potionProb potion = productProb (== potion)

signProb :: Sign -> Ingredient -> Ingredient -> [AlchemicalAssignment] -> R.Rational
signProb sign = productProb ((==) sign . potionSign)
