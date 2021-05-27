module Main where

import App (app)
import qualified AppF as F
import qualified Delivery.Domain.Configuration.Config as DC
import qualified Delivery.Domain.Dsl as Dsl
import Files.Dsl (mkCustom, mkDirectory)

main :: IO ()
main = do
  --_ <- F.runApp F.evalLogger ""
  let input = mkDirectory "/Users/will/Desktop/in"
      ext = mkCustom ".txt"
      output = mkDirectory "/Users/will/Desktop/out2"
      config' = DC.config "/Users/will/Desktop/in" "in" ".txt" 2
  --f <- F.runApp $ F.getFiles "GetFiles" input ext
  --_ <- F.runApp $ F.createDirectory "CreateDir" output
  --let saved = (\f -> F.runApp (F.writeFile "WriteFile" f output)) <$> f
  --sequence_ saved
  --print f
  drones <- F.runApp $ F.basicProgram "BasicProgram" config'
  print drones

--main = app
