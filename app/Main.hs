module Main where

import DimAnalysis
import System.Environment (getArgs)
import qualified Text.PrettyPrint as PP

main :: IO ()
main = do
  eqnStr <- head <$> getArgs
  case mainParser eqnStr of
    Left e -> putStrLn e
    Right eqn -> do
      case solveEquation eqn of
        Left e -> putStrLn $ "error: " <> e
        Right r -> putStrLn $ PP.render (ppExpr r)
