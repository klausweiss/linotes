import qualified Parametrized as P
import qualified Unit as U

main :: IO ()
main = U.main >>
       P.main
