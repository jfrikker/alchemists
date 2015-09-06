{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module: Tools.Game.Alchemists.Reagents

Everything related to the deduction grid. This module basically has 4 types of things:

  1. Data types representing alchemicals, ingredients, potions, etc.
  
  2. Facts about those types. For example, the color of a potion, or the potion created by combining two alchemicals.
  
  3. The set of all possible (Ingredient, Alchemical) assignments, and tools for filtering them based on the game state.
  
  4. Tools for extracting useful information from the set of states arrived at in step (3).
-}

module Tools.Game.Alchemists.Reagents (
  -- * Datatypes
  Alchemical(..),
  Ingredient(..),
  Potion(..),
  Color(..),
  Sign(..),

  -- * Basic facts
  potionColor,
  potionSign,
  oppositeSign,
  potentialAlchemicalProduct,
  alchemicalProduct,

  -- * Ingredient / Alchemical assignments
  PerIngredient,
  PerAlchemical,
  AlchemicalAssignment,
  allAssignments,

  -- * Filtering assignments
  AlchemicalConstraint,
  createsPotion,
  salesResult,
  debunkResult,

  -- * Extracting facts from potential assignments
  alchemicalProbs,
  potionProb,
  signProb
) where

import qualified Tools.Game.Alchemists.PerEnum as PE
import qualified Data.List as L
import qualified Data.Ratio as R
import Data.Ratio ((%))

-- |The 7 potions.
data Potion = BLUE_PLUS | BLUE_MINUS | RED_PLUS | RED_MINUS | GREEN_PLUS | GREEN_MINUS | NEUTRAL deriving (Show, Eq)

-- |The 3 potion colors.
data Color = BLUE | RED | GREEN deriving (Show)

-- |The 3 potion signs. All potion types have a sign, except for the NEUTRAL potion, which has NO_SIGN.
data Sign = PLUS | MINUS | NO_SIGN deriving (Show, Eq)

-- |The 8 alchemicals. The alchemical types are difficult to name. They follow the order shown on the deduction grid,
-- top to bottom.
data Alchemical = AL_1 | AL_2 | AL_3 | AL_4 | AL_5 | AL_6 | AL_7 | AL_8 deriving (Show, Eq, Enum, Bounded)

-- |The 8 ingredients.
data Ingredient = MUSHROOM | SPROUT | TOAD | FOOT | FLOWER | ROOT | SCORPION | FEATHER deriving (Show, Eq, Enum, Bounded)

-- |A potion's color.
potionColor :: Potion -> Color
potionColor BLUE_PLUS = BLUE
potionColor BLUE_MINUS = BLUE
potionColor RED_PLUS = RED
potionColor RED_MINUS = RED
potionColor GREEN_PLUS = GREEN
potionColor GREEN_MINUS = GREEN

-- |A potion's sign.
potionSign :: Potion -> Sign
potionSign BLUE_PLUS = PLUS
potionSign BLUE_MINUS = MINUS
potionSign RED_PLUS = PLUS
potionSign RED_MINUS = MINUS
potionSign GREEN_PLUS = PLUS
potionSign GREEN_MINUS = MINUS
potionSign NEUTRAL = NO_SIGN

-- |The inverse of a sign (so a PLUS becomes a MINUS, NO_SIGN remains NO_SIGN).
oppositeSign :: Sign -> Sign
oppositeSign PLUS = MINUS
oppositeSign MINUS = PLUS
oppositeSign NO_SIGN = NO_SIGN

-- |The sign of each color for each alchemical.
potentialAlchemicalProduct :: Alchemical -> Potion -> Bool
potentialAlchemicalProduct AL_1 BLUE_PLUS = False
potentialAlchemicalProduct AL_1 RED_PLUS = False
potentialAlchemicalProduct AL_1 GREEN_PLUS = True
potentialAlchemicalProduct AL_2 BLUE_PLUS = True
potentialAlchemicalProduct AL_2 RED_PLUS = True
potentialAlchemicalProduct AL_2 GREEN_PLUS = False
potentialAlchemicalProduct AL_3 BLUE_PLUS = False
potentialAlchemicalProduct AL_3 RED_PLUS = True
potentialAlchemicalProduct AL_3 GREEN_PLUS = False
potentialAlchemicalProduct AL_4 BLUE_PLUS = True
potentialAlchemicalProduct AL_4 RED_PLUS = False
potentialAlchemicalProduct AL_4 GREEN_PLUS = True
potentialAlchemicalProduct AL_5 BLUE_PLUS = True
potentialAlchemicalProduct AL_5 RED_PLUS = False
potentialAlchemicalProduct AL_5 GREEN_PLUS = False
potentialAlchemicalProduct AL_6 BLUE_PLUS = False
potentialAlchemicalProduct AL_6 RED_PLUS = True
potentialAlchemicalProduct AL_6 GREEN_PLUS = True
potentialAlchemicalProduct AL_7 BLUE_PLUS = False
potentialAlchemicalProduct AL_7 RED_PLUS = False
potentialAlchemicalProduct AL_7 GREEN_PLUS = False
potentialAlchemicalProduct AL_8 BLUE_PLUS = True
potentialAlchemicalProduct AL_8 RED_PLUS = True
potentialAlchemicalProduct AL_8 GREEN_PLUS = True
potentialAlchemicalProduct a BLUE_MINUS = not $ potentialAlchemicalProduct a BLUE_PLUS
potentialAlchemicalProduct a RED_MINUS = not $ potentialAlchemicalProduct a RED_PLUS
potentialAlchemicalProduct a GREEN_MINUS = not $ potentialAlchemicalProduct a GREEN_PLUS

-- |The potion produced by the combination of two alchemicals. Combining an alchemical with itself is not possible.
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

-- |A data structure that can store a separate value for each of the 8 ingredients.
data PerIngredient a = PI a a a a a a a a deriving (Show)

instance Functor PerIngredient where
  fmap f (PI a b c d e f' g h) = PI (f a) (f b) (f c) (f d) (f e) (f f') (f g) (f h)

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

-- |The assignment of all 8 ingredients to specific alchemicals.
type AlchemicalAssignment = PerIngredient Alchemical

-- |A data structure that can store a separate value for each of the 8 alchemicals.
data PerAlchemical a = PA a a a a a a a a deriving (Show)

instance Functor PerAlchemical where
  fmap f (PA a b c d e f' g h) = PA (f a) (f b) (f c) (f d) (f e) (f f') (f g) (f h)

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

-- |A constraint on the set of all alchemical assignments. This is normally used to filter the remaining set of 
-- possible assignments. For example:
--
-- > filter (createsPotion FOOT MUSHROOM BLUE_PLUS) allAssignments
type AlchemicalConstraint = AlchemicalAssignment -> Bool

ingredientProduct :: Ingredient -> Ingredient -> AlchemicalAssignment -> Potion
ingredientProduct i1 i2 assignment = alchemicalProduct (PE.get i1 assignment) (PE.get i2 assignment)

-- |All possible ingredient / alchemical assignments. There are 40,320 such assignments.
allAssignments :: [AlchemicalAssignment]
allAssignments = map assignmentFromList $ L.permutations [AL_1 .. AL_8]

assignmentFromList :: [Alchemical] -> AlchemicalAssignment
assignmentFromList [al1, al2, al3, al4, al5, al6, al7, al8] = PI al1 al2 al3 al4 al5 al6 al7 al8

-- |A constraint requiring that two specific ingredients mix to make a specific type of potion. Useful when you have
-- just mixed a new potion.
createsPotion :: Ingredient -> Ingredient -> Potion -> AlchemicalConstraint
createsPotion i1 i2 potion assignment = potion == ingredientProduct i1 i2 assignment

-- |A constraint indicating that an attempt at a potion sale had a certain result. The result (the Int parameter) is
-- represented by the base payment amount of the outcome.
--
-- Of course,
--
-- > salesResult i1 i2 potion 4 assignment == createsPotion i1 i2 potion assignment
--
-- and
--
-- > salesResult i1 i2 potion 2 assignment == createsPotion i1 i2 NEUTRAL assignment
salesResult :: Ingredient -> Ingredient -> Potion -> Int -> AlchemicalConstraint
salesResult i1 i2 potion result assignment = case result of
  4 -> potion == product
  3 -> potion /= product && potionSign potion == potionSign product
  2 -> product == NEUTRAL
  1 -> (oppositeSign $ potionSign potion) == potionSign product
  where product = ingredientProduct i1 i2 assignment

-- |A constraint indicating that a certain ingredient can make a certain type of potion. This is normally discovered
-- via a debunk attempt.
debunkResult :: Ingredient -> Potion -> AlchemicalConstraint
debunkResult i p a = potentialAlchemicalProduct (PE.get i a) p

-- |Given an ingredient and a list of potential assignments, computes the odds that that ingredient is assigned to
-- each alchemical. This function takes an ingredient, rather than an ingredient and an alchemical, for efficiency.
alchemicalProbs :: Ingredient -> [AlchemicalAssignment] -> PerAlchemical R.Rational
alchemicalProbs i assignments = foldr inc (PE.init (0%1)) assignments
  where inc = PE.update (+ 1%len) . PE.get i
        len = fromIntegral $ length assignments

productProb :: (Potion -> Bool) -> Ingredient -> Ingredient -> [AlchemicalAssignment] -> R.Rational
productProb f i1 i2 assignments = foldr inc (0%1) assignments / fromIntegral (length assignments)
  where inc assignment count | f $ ingredientProduct i1 i2 assignment = count + (1%1)
                             | otherwise = count

-- |Returns the odds that two specific ingredients will mix to form a certain potion.
potionProb :: Potion -> Ingredient -> Ingredient -> [AlchemicalAssignment] -> R.Rational
potionProb potion = productProb (== potion)

-- |Returns the odds that two specific ingredients will mix to form a potion with a certain sign.
signProb :: Sign -> Ingredient -> Ingredient -> [AlchemicalAssignment] -> R.Rational
signProb sign = productProb ((==) sign . potionSign)
