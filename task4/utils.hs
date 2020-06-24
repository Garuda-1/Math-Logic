module Utils where

import Parser
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set



getVarNames :: Exp -> Set.Set String
getVarNames (EImpl x y) = Set.union (getVarNames x) (getVarNames y)
getVarNames (EDisj x y) = Set.union (getVarNames x) (getVarNames y)
getVarNames (EConj x y) = Set.union (getVarNames x) (getVarNames y)
getVarNames (ENeg x) = getVarNames x
getVarNames (EVar name) = Set.singleton name


type VarMap = Map.Map String Bool

evaluate :: VarMap -> Exp -> Bool
evaluate map (EImpl x y) = (not $ evaluate map x) || (evaluate map y)
evaluate map (EDisj x y) = (evaluate map x) || (evaluate map y)
evaluate map (EConj x y) = (evaluate map x) && (evaluate map y)
evaluate map (ENeg x) = not $ evaluate map x
evaluate map (EVar name) =
  case (Map.lookup name map) of
    Just v -> v
    Nothing -> error $ "Variable " ++ name ++ " has no assigned value."


necessaryVars :: [String] -> Bool -> Exp -> Maybe [String]
necessaryVars vars mode exp =
  let
    basicVarMap :: VarMap
    basicVarMap = foldl (\map name -> Map.insert name mode map) Map.empty vars

    collector :: VarMap -> [String] -> Exp -> Maybe [String]
    collector _ [] _ = Just []
    collector map (name : names) exp =
      if ((evaluate (Map.insert name True map) exp) && 
          (evaluate (Map.insert name False map) exp)) then
        collector map names exp
      else if (evaluate (Map.insert name mode map) exp) then
        case (collector map names exp) of
          Nothing -> Nothing
          Just list -> Just $ name : list
      else
        Nothing
  in
    collector basicVarMap vars exp
    
defineSolution :: Exp -> Maybe (Bool, [Exp])
defineSolution exp =
  case (necessaryVars (Set.toList $ getVarNames exp) True exp) of
    Just names -> Just (True, map (\name -> (EVar name)) names)
    Nothing -> case (necessaryVars (Set.toList $ getVarNames exp) True exp) of
      Just names -> Just (False, map (\name -> (ENeg (EVar name))) names)
      Nothing -> Nothing

