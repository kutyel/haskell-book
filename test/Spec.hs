import qualified Test.Chapter14  as Chapter14
import qualified Test.Chapter8   as Chapter8
import           Test.Hspec
import qualified Test.WordNumber as WordNumber

main :: IO ()
main =
  hspec $ do
    Chapter8.spec
    WordNumber.spec
    Chapter14.spec
