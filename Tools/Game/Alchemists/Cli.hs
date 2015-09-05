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
formatPercent r = printf "%5.1f%%" (100 * (realToFrac r :: Double))

printAlchemicalProbs :: [AlchemicalAssignment] -> IO ()
printAlchemicalProbs a = printPETable $ PET.tmap formatPercent probs
  where probs = PE.build $ flip alchemicalProbs a :: PerIngredient (PerAlchemical R.Rational)

printIxIProbTable :: (Ingredient -> Ingredient -> R.Rational) -> IO ()
printIxIProbTable f = printPETable $ PET.tmap formatPercent table
  where table = PET.build f :: PerIngredient (PerIngredient R.Rational)

printPotionProbs :: Potion -> [AlchemicalAssignment] -> IO ()
printPotionProbs potion assignments = printIxIProbTable cellValue
  where cellValue i1 i2 | i1 == i2 = 0%1
                        | otherwise = potionProb potion i1 i2 assignments

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
