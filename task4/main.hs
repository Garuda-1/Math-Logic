module Main where

import Utils
import Parser
import Lex
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


convertToTautology :: Exp -> Bool -> [Exp] -> Exp
convertToTautology exp mode gamma = 
  let
    basicExp :: Exp
    basicExp = if (mode) then exp else (ENeg exp)
  in
    foldr (\e1 e2 -> (EImpl e1 e2)) basicExp gamma

getBasicVarMap :: [Exp] -> Bool -> VarMap
getBasicVarMap exps mode = Map.fromList (map (\(EVar e) -> (e, mode)) exps)


main :: IO ()
main = do
  targetStr <- getLine
  let target = (calc . alexScanTokens) targetStr
  case (defineSolution target) of
    Nothing -> putStrLn ":("
    Just (mode, gamma) -> do
      let basicGamma = (map (\s -> if mode then (EVar s) else (ENeg (EVar s))) (Set.toList $ getVarNames target))
      let actualTarget = (if mode then target else (ENeg target))
      let tautology = convertToTautology actualTarget mode gamma
      let proof = totalProof basicGamma [] Map.empty tautology
      mapM_ putStr $ map (\e -> (show e ++ ", ")) basicGamma 
      putStr "|- "
      putStrLn $ show tautology
      mapM_ (putStrLn . show) $ proof

