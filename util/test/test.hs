import           Universum

import           Spec (spec)
import           Test.Hspec (hspec)
import           Test.Pos.Util.AesonTripping (runTests, tests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Util.AesonTripping.tests
        ]
