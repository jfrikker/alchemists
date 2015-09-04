module Tools.Game.Alchemists.Cli (
  printAlchemicalProbs,
  printPotionProbs
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
printAlchemicalProbs a = printPETable formatPercent probs
  where probs = PE.build $ flip alchemicalProbs a :: PerIngredient (PerAlchemical R.Rational)

printPotionProbs :: Potion -> [AlchemicalAssignment] -> IO ()
printPotionProbs potion assignments = printPETable formatPercent table
  where table = PET.build cellValue :: PerIngredient (PerIngredient R.Rational)
        cellValue i1 i2 | i1 == i2 = 0%1
                        | otherwise = potionProb potion i1 i2 assignments

printTable :: (Show r, Show c) => (r -> c -> String) -> [r] -> [c] -> IO ()
printTable cellValue rows cols = do
  let left = Box.vcat Box.right $ Box.char ' ' : map (Box.text . show) rows
  let dataCols = map layoutCol cols
  Box.printBox $ Box.hsep 1 Box.left $ left : dataCols
  where layoutCol col = Box.vcat Box.right $ Box.text (show col) : map (Box.text . (`cellValue` col)) rows

printPETable :: (PE.PerEnum p1 c, PE.PerEnum p2 r, Show c, Show r) => (a -> String) -> p1 (p2 a) -> IO ()
printPETable formatCell table = printTable cellValue (PET.rowKeys table) (PET.colKeys table)
  where cellValue r c = formatCell $ PET.get c r table
