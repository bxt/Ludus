import Data.List
import Control.Monad (ap)

isSelfDescribing n = sn == desc sn
  where desc = concatMap (show . pred . length) . group . sort . (['0'..to] ++)
        to   = head $ show $ pred $ length sn
        sn   = show n

selfDescribing = filter isSelfDescribing [1 ..]

main = print selfDescribing
