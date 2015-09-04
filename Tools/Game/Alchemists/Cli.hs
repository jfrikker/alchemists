module Tools.Game.Alchemists.Cli (
  printAlchemicalProbs
) where

import Tools.Game.Alchemists.Reagents
import qualified Tools.Game.Alchemists.PerEnum as PE
import qualified Tools.Game.Alchemists.PerEnumTable as PET
import qualified Text.PrettyPrint.Boxes as Box
import Text.PrettyPrint.Boxes ((//))
import Text.Printf(printf)
import qualified Data.Ratio as R

column :: [String] -> Box.Box
column = foldr ((//) . Box.text) Box.nullBox

formatPercent :: R.Rational -> String
formatPercent r = printf "%.1f%%" (100 * (realToFrac r :: Double))

printAlchemicalProbs :: [AlchemicalAssignment] -> IO ()
printAlchemicalProbs a = printPETable formatPercent probs
  where probs = PE.build $ flip alchemicalProbs a :: PerIngredient (PerAlchemical R.Rational)

printTable :: (Show r, Show c) => (r -> c -> String) -> [r] -> [c] -> IO ()
printTable cellValue rows cols = do
  let left = Box.char ' ' // column (map show rows)
  let dataCols = map layoutCol cols
  Box.printBox $ Box.hsep 1 Box.left $ left : dataCols
  where layoutCol col = Box.text (show col) // column (map (`cellValue` col) rows)

printPETable :: (PE.PerEnum p1 c, PE.PerEnum p2 r, Show c, Show r) => (a -> String) -> p1 (p2 a) -> IO ()
printPETable formatCell table = printTable cellValue (PET.rowKeys table) (PET.colKeys table)
  where cellValue r c = formatCell $ PET.get c r table
