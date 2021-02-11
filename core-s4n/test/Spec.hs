import Control.Monad ((>>))
import Core.DslSpec (spec)
import Test.Hspec (hspec)

main :: IO ()
main =
  putStrLn "Init core test.."
    >> hspec spec
      <* putStrLn "End core test"
