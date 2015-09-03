module Tools.Game.Alchemists.Reagents (
  Potion(..),
  Alchemical(..),
  Ingredient(..),
  PerIngredient,
  AlchemicalAssignment,
  AlchemicalConstraint,
  potionColor,
  potionSign,
  alchemicalProduct,
  forAssignment,
  allAssignments,
  createsPotion
) where

import qualified Data.List as L

data Potion = BLUE_PLUS | BLUE_MINUS | RED_PLUS | RED_MINUS | GREEN_PLUS | GREEN_MINUS | NEUTRAL deriving (Show, Eq)

data Color = BLUE | RED | GREEN deriving (Show)

data Sign = PLUS | MINUS deriving (Show)

data Alchemical = AL_1 | AL_2 | AL_3 | AL_4 | AL_5 | AL_6 | AL_7 | AL_8 deriving (Show, Eq, Enum)

data Ingredient = MUSHROOM | SPROUT | TOAD | FOOT | FLOWER | ROOT | SCORPION | FEATHER deriving (Show, Eq)

type PerIngredient a = (a, a, a, a, a, a, a, a)

type AlchemicalAssignment = PerIngredient Alchemical

type AlchemicalConstraint = AlchemicalAssignment -> Bool

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

forAssignment :: Ingredient -> PerIngredient a -> a
forAssignment MUSHROOM (x, _, _, _, _, _, _, _) = x
forAssignment SPROUT (_, x, _, _, _, _, _, _) = x
forAssignment TOAD (_, _, x, _, _, _, _, _) = x
forAssignment FOOT (_, _, _, x, _, _, _, _) = x
forAssignment FLOWER (_, _, _, _, x, _, _, _) = x
forAssignment ROOT (_, _, _, _, _, x, _, _) = x
forAssignment SCORPION (_, _, _, _, _, _, x, _) = x
forAssignment FEATHER (_, _, _, _, _, _, _, x) = x

setForAssignment :: Ingredient -> a -> PerIngredient a -> PerIngredient a
setForAssignment MUSHROOM val (_, b, c, d, e, f, g, h) = (val, b, c, d, e, f, g, h)
setForAssignment SPROUT val (a, _, c, d, e, f, g, h) = (a, val, c, d, e, f, g, h)
setForAssignment TOAD val (a, b, _, d, e, f, g, h) = (a, b, val, d, e, f, g, h)
setForAssignment FOOT val (a, b, c, _, e, f, g, h) = (a, b, c, val, e, f, g, h)
setForAssignment FLOWER val (a, b, c, d, _, f, g, h) = (a, b, c, d, val, f, g, h)
setForAssignment ROOT val (a, b, c, d, e, _, g, h) = (a, b, c, d, e, val, g, h)
setForAssignment SCORPION val (a, b, c, d, e, f, _, h) = (a, b, c, d, e, f, val, h)
setForAssignment FEATHER val (a, b, c, d, e, f, g, _) = (a, b, c, d, e, f, g, val)

ingredientProduct :: Ingredient -> Ingredient -> AlchemicalAssignment -> Potion
ingredientProduct i1 i2 assignment = alchemicalProduct (forAssignment i1 assignment) (forAssignment i2 assignment)

allAssignments :: [AlchemicalAssignment]
allAssignments = map assignmentFromList $ L.permutations [AL_1 .. AL_8]

assignmentFromList :: [Alchemical] -> AlchemicalAssignment
assignmentFromList [al1, al2, al3, al4, al5, al6, al7, al8] = (al1, al2, al3, al4, al5, al6, al7, al8)

createsPotion :: Ingredient -> Ingredient -> Potion -> AlchemicalConstraint
createsPotion i1 i2 potion assignment = potion == ingredientProduct i1 i2 assignment
