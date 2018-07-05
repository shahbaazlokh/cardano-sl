module Test.Pos.Util.TempTH where

import           Hedgehog (Group, discoverPrefix)
import           Hedgehog.Internal.TH (TExpQ)

discoverGolden :: TExpQ Group
discoverGolden = discoverPrefix "golden_"

