{-|
  Module: Tools.Game.Alchemists.Cli

  This module contains tools that make it easy to use this package from the repl. For example:

  >>> let a1 = filter (createsPotion TOAD FLOWER BLUE_PLUS) allAssignments
  >>> let a2 = filter (createsPotion MUSHROOM FOOT RED_MINUS) a1
  >>> printAlchemicalProbs a2
       MUSHROOM SPROUT   TOAD   FOOT FLOWER   ROOT SCORPION FEATHER
  AL_1    37.5%   6.2%         37.5%          6.2%     6.2%    6.2%
  AL_2           12.5%  25.0%         25.0%  12.5%    12.5%   12.5%
  AL_3           25.0%                       25.0%    25.0%   25.0%
  AL_4    12.5%   6.2%  25.0%  12.5%  25.0%   6.2%     6.2%    6.2%
  AL_5    12.5%   6.2%  25.0%  12.5%  25.0%   6.2%     6.2%    6.2%
  AL_6           25.0%                       25.0%    25.0%   25.0%
  AL_7    37.5%   6.2%         37.5%          6.2%     6.2%    6.2%
  AL_8           12.5%  25.0%         25.0%  12.5%    12.5%   12.5%

  All of the functions that print tables of probabilities (like the one shown above) omit cells whose probability is 0
  to avoid clutter.
-}
module Tools.Game.Alchemists.Cli (
  printAlchemicalProbs,
  printPotionProbs,
  printSignProbs
) where

import Tools.Game.Alchemists.Reagents
import qualified Tools.Game.Alchemists.PerEnum as PE
import qualified Tools.Game.Alchemists.PerEnumTable as PET
import qualified Text.PrettyPrint.Boxes as Box
import Text.Printf(printf)
import qualified Data.Ratio as R
import Data.Ratio ((%))

formatPercent :: R.Rational -> String
formatPercent r | r == (0%1) = ""
                | otherwise = printf "%5.1f%%" (100 * (realToFrac r :: Double))

-- |Prints the odds that each ingredient is assigned to each alchemical. The format is a table organized just like the
-- deduction grid provided with the game.
--
-- >>> printAlchemicalProbs a2
--      MUSHROOM SPROUT   TOAD   FOOT FLOWER   ROOT SCORPION FEATHER
-- AL_1    37.5%   6.2%         37.5%          6.2%     6.2%    6.2%
-- AL_2           12.5%  25.0%         25.0%  12.5%    12.5%   12.5%
-- AL_3           25.0%                       25.0%    25.0%   25.0%
-- AL_4    12.5%   6.2%  25.0%  12.5%  25.0%   6.2%     6.2%    6.2%
-- AL_5    12.5%   6.2%  25.0%  12.5%  25.0%   6.2%     6.2%    6.2%
-- AL_6           25.0%                       25.0%    25.0%   25.0%
-- AL_7    37.5%   6.2%         37.5%          6.2%     6.2%    6.2%
-- AL_8           12.5%  25.0%         25.0%  12.5%    12.5%   12.5%
printAlchemicalProbs :: [AlchemicalAssignment] -> IO ()
printAlchemicalProbs a = printPETable $ PET.tmap formatPercent probs
  where probs = PE.build $ flip alchemicalProbs a :: PerIngredient (PerAlchemical R.Rational)

printIxIProbTable :: (Ingredient -> Ingredient -> R.Rational) -> IO ()
printIxIProbTable f = printPETable $ PET.tmap formatPercent table
  where table = PET.build f :: PerIngredient (PerIngredient R.Rational)

-- |Prints the odds that each pair of ingredients will combine to make a certain potion type. The format is a table
-- where the rows and columns are both ingredients.
--
-- >>> printPotionProbs GREEN_PLUS a2
--          MUSHROOM SPROUT   TOAD   FOOT FLOWER   ROOT SCORPION FEATHER
-- MUSHROOM           14.1%  21.9%         21.9%  14.1%    14.1%   14.1%
--   SPROUT    14.1%         14.1%  14.1%  14.1%  14.6%    14.6%   14.6%
--     TOAD    21.9%  14.1%         21.9%         14.1%    14.1%   14.1%
--     FOOT           14.1%  21.9%         21.9%  14.1%    14.1%   14.1%
--   FLOWER    21.9%  14.1%         21.9%         14.1%    14.1%   14.1%
--     ROOT    14.1%  14.6%  14.1%  14.1%  14.1%           14.6%   14.6%
-- SCORPION    14.1%  14.6%  14.1%  14.1%  14.1%  14.6%            14.6%
--  FEATHER    14.1%  14.6%  14.1%  14.1%  14.1%  14.6%    14.6%

printPotionProbs :: Potion -> [AlchemicalAssignment] -> IO ()
printPotionProbs potion assignments = printIxIProbTable cellValue
  where cellValue i1 i2 | i1 == i2 = 0%1
                        | otherwise = potionProb potion i1 i2 assignments

-- |Prints the odds that each pair of ingredients will combine to make a potion with a certain sign. The format is a
-- table where the rows and columns are both ingredients.
--
-- >>> printSignProbs MINUS a2
--          MUSHROOM SPROUT   TOAD   FOOT FLOWER   ROOT SCORPION FEATHER
-- MUSHROOM           64.1%  46.9% 100.0%  46.9%  64.1%    64.1%   64.1%
--   SPROUT    64.1%         26.6%  64.1%  26.6%  31.2%    31.2%   31.2%
--     TOAD    46.9%  26.6%         46.9%         26.6%    26.6%   26.6%
--     FOOT   100.0%  64.1%  46.9%         46.9%  64.1%    64.1%   64.1%
--   FLOWER    46.9%  26.6%         46.9%         26.6%    26.6%   26.6%
--     ROOT    64.1%  31.2%  26.6%  64.1%  26.6%           31.2%   31.2%
-- SCORPION    64.1%  31.2%  26.6%  64.1%  26.6%  31.2%            31.2%
--  FEATHER    64.1%  31.2%  26.6%  64.1%  26.6%  31.2%    31.2%
printSignProbs :: Sign -> [AlchemicalAssignment] -> IO ()
printSignProbs sign assignments = printIxIProbTable cellValue
  where cellValue i1 i2 | i1 == i2 = 0%1
                        | otherwise = signProb sign i1 i2 assignments

printTable :: (Show r, Show c) => (r -> c -> String) -> [r] -> [c] -> IO ()
printTable cellValue rows cols = do
  let left = Box.vcat Box.right $ Box.char ' ' : map (Box.text . show) rows
  let dataCols = map layoutCol cols
  Box.printBox $ Box.hsep 1 Box.left $ left : dataCols
  where layoutCol col = Box.vcat Box.right $ Box.text (show col) : map (Box.text . (`cellValue` col)) rows

printEnumTable :: (Show r, Bounded r, Enum r, Show c, Bounded c, Enum c) => (r -> c -> String) -> IO ()
printEnumTable f = printTable f [minBound .. maxBound] [minBound .. maxBound]

printPETable :: (PE.PerEnum p1 c, PE.PerEnum p2 r, Show c, Show r) => p1 (p2 String) -> IO ()
printPETable table = printEnumTable cellValue
  where cellValue r c = PET.get c r table
