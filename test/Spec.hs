import qualified Test.Chapter14  as Chapter14
import qualified Test.Chapter15  as Chapter15
import qualified Test.Chapter16  as Chapter16
import qualified Test.Chapter8   as Chapter8
import qualified Test.Cipher     as Cipher
import           Test.Hspec
import qualified Test.WordNumber as WordNumber

main :: IO ()
main =
  hspec $ do
    Cipher.spec
    WordNumber.spec
    Chapter8.spec
    Chapter14.spec
    Chapter15.spec
    Chapter16.spec
