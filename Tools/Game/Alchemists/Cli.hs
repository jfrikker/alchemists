module Tools.Game.Alchemists.Cli (
  printAlchemicalProbs
) where

import Tools.Game.Alchemists.Reagents
import qualified Tools.Game.Alchemists.PerEnum as PE
import qualified Text.PrettyPrint.Boxes as Box
import Text.PrettyPrint.Boxes ((//))
import Text.Printf(printf)

column :: [String] -> Box.Box
column = foldr ((//) . Box.text) Box.nullBox

printAlchemicalProbs :: [AlchemicalAssignment] -> IO ()
printAlchemicalProbs a = do
  let left = Box.char ' ' // column (map show ([minBound .. maxBound] :: [Alchemical]))
  let dataCols = map layoutCol [minBound .. maxBound]
  Box.printBox $ Box.hsep 1 Box.left $ left : dataCols
  where layoutCol i = let probs = alchemicalProbs i a
                      in Box.text (show i) // column (map (formatCell . (`PE.get` probs)) [minBound .. maxBound])
        formatCell r = printf "%.1f%%" (100 * (realToFrac r :: Double))
