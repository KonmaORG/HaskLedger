-- Oracle: ASG only
module Main (main) where

import Data.Map qualified as Map
import Data.Vector qualified as Vector
import Covenant.ASG (ASG (ASG), defaultDatatypes, runASGBuilder)
import Covenant.ArgDict (ppASG)
import HaskLedger.Contract (Validator (Validator))
import Oracle (oracle)

main :: IO ()
main = do
  let Validator _name builder = oracle
  case runASGBuilder defaultDatatypes builder of
    Left e -> putStrLn $ "ASG error: " <> show e
    Right (ASG (rootId, asgMap)) -> do
      putStrLn $ "root=" <> show rootId <> " nodes=" <> show (Map.size asgMap)
      putStrLn $ ppASG asgMap
