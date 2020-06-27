module Main where

import Utils
import Parser
import Lex
import Proofs
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set



convertToTautology :: Exp -> Bool -> [Exp] -> Exp
convertToTautology exp mode gamma = foldr (\e1 e2 -> (EImpl e1 e2)) exp gamma


getBasicVarMap :: [Exp] -> Bool -> VarMap
getBasicVarMap exps mode = Map.fromList (map (\(EVar e) -> (e, mode)) exps)


unfoldGamma :: [Exp] -> Exp -> [Exp]
unfoldGamma (gamma@(EVar name0) : gammas) (EImpl (EVar name1) x)
  | name0 == name1 = [gamma, x] ++ unfoldGamma gammas x
  | otherwise = []
unfoldGamma (gamma@(ENeg (EVar name0)) : gammas) (EImpl (ENeg (EVar name1)) x)
  | name0 == name1 = [gamma, x] ++ unfoldGamma gammas x
  | otherwise = []
unfoldGamma [] _ = []
unfoldGamma gamma exp = 
  error $ "Unfolding error, gamma = " ++ show gamma ++ ", exp = " ++ show exp


main :: IO ()
main = do
  targetStr <- getLine
  let target = (calc . alexScanTokens) targetStr
  case (defineSolution target) of
    Nothing -> putStrLn ":("
    Just (mode, gamma) -> do
      let basicGamma = (map (\s -> (EVar s)) (Set.toList $ getVarNames target))
      let actualTarget = (if mode then target else (ENeg target))
      let tautology = convertToTautology actualTarget mode gamma
      let proof = totalProof basicGamma [] Map.empty tautology
      putStr $ intercalate ", " $ map show gamma 
      if (null gamma) then
        putStr "|- "
      else
        putStr " |- "
      putStrLn $ show actualTarget
      mapM_ (putStrLn . show) $ proof
      mapM_ (putStrLn . show) $ unfoldGamma gamma tautology

