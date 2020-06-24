module Main where

import Utils
import Parser
import Lex


main :: IO ()
main = do
  targetStr <- getLine
  let target = (calc . alexScanTokens) targetStr

  case (defineSolution target) of
    Just (_, gamma) -> putStrLn $ show gamma
    Nothing -> putStrLn ":("
