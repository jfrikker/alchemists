module Tools.Game.Alchemists.Cli (
  printReagentProbs
) where

import Tools.Game.Alchemists.Reagents
import qualified Tools.Game.Alchemists.PerEnum as PE
import qualified Text.PrettyPrint.Boxes as Box
import Text.PrettyPrint.Boxes ((//))
import Text.Printf(printf)

column :: [String] -> Box.Box
column = foldr ((//) . Box.text) Box.nullBox

printReagentProbs :: [AlchemicalAssignment] -> IO ()
printReagentProbs a = do
  let left = Box.char ' ' // column (map show ([minBound .. maxBound] :: [Alchemical]))
  let dataCols = map layoutCol [minBound .. maxBound]
  Box.printBox $ Box.hsep 1 Box.left $ left : dataCols
  where probs = reagentProbs a
        layoutCol i = Box.text (show i) // column (map (formatCell . cellValue i) [minBound .. maxBound])
        cellValue i al = PE.get al $ PE.get i probs
        formatCell r = printf "%.1f%%" (100 * (realToFrac r :: Double))
