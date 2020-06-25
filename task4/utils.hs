module Utils where

import Parser
import Proofs
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
    targetExp :: Exp
    targetExp = if mode then exp else (ENeg exp)

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
    collector basicVarMap vars targetExp
    

defineSolution :: Exp -> Maybe (Bool, [Exp])
defineSolution exp =
  case (necessaryVars (Set.toList $ getVarNames exp) True exp) of
    Just names -> Just (True, map (\name -> (EVar name)) names)
    Nothing -> case (necessaryVars (Set.toList $ getVarNames exp) False exp) of
      Just names -> Just (False, map (\name -> (ENeg (EVar name))) names)
      Nothing -> Nothing


generateProof :: Exp -> VarMap -> (Bool, [Exp])
generateProof (EImpl a b) varmap = 
  let 
    res1 :: (Bool, [Exp])
    res1 = generateProof a varmap

    res2 :: (Bool, [Exp])
    res2 = generateProof b varmap
  in 
    ((not $ fst res1) || (fst res2), 
      ((snd res1) ++ (snd res2) ++ (proveImpl a (fst res1) b (fst res2))))
generateProof (EDisj a b) varmap = 
  let 
    res1 :: (Bool, [Exp])
    res1 = generateProof a varmap

    res2 :: (Bool, [Exp])
    res2 = generateProof b varmap
  in 
    ((fst res1) || (fst res2), 
      ((snd res1) ++ (snd res2) ++ (proveOr a (fst res1) b (fst res2))))
generateProof (EConj a b) varmap =
  let 
    res1 :: (Bool, [Exp])
    res1 = generateProof a varmap

    res2 :: (Bool, [Exp])
    res2 = generateProof b varmap
  in 
    ((fst res1) && (fst res2), 
      ((snd res1) ++ (snd res2) ++ (proveAnd a (fst res1) b (fst res2))))
generateProof (ENeg a) varmap =
  let
    res :: (Bool, [Exp]) 
    res = generateProof a varmap
  in
    ((not $ fst res), ((snd res) ++ (proveNeg a (fst res))))
generateProof (EVar v) varmap = 
  case (Map.lookup v varmap) of
    Just value -> 
      if (value) then 
        --(True, [(EVar v)]) 
        (True, [])
      else 
        --(False, [(ENeg (EVar v))])
        (False, [])
    Nothing -> error $ "Invalid variable map, no value for " ++ v

varmapToGammas :: VarMap -> [Exp]
varmapToGammas varmap = 
  map (\p -> if (snd p) then (EVar $ fst p) else (ENeg (EVar $ fst p))) (Map.toList varmap)

generateAllProof :: Exp -> VarMap -> [Exp]
generateAllProof exp varmap =
  (varmapToGammas varmap) ++ (snd $ generateProof exp varmap)


proveThirdExcluded :: Exp -> [Exp]
proveThirdExcluded exp =
  [ (EImpl exp (EDisj exp (ENeg exp))) ] ++
  proveCounterposition (EImpl exp (EDisj exp (ENeg exp))) ++
  [ (EImpl (ENeg (EDisj exp (ENeg exp))) (ENeg exp))
  , (EImpl (ENeg exp) (EDisj exp (ENeg exp))) ] ++
  proveCounterposition (EImpl (ENeg exp) (EDisj exp (ENeg exp))) ++
  [ (EImpl (ENeg (EDisj exp (ENeg exp))) (ENeg (ENeg exp))) 
  , (EImpl
      (EImpl (ENeg (EDisj exp (ENeg exp))) (ENeg exp))
      (EImpl
        (EImpl (ENeg (EDisj exp (ENeg exp))) (ENeg (ENeg exp)))
        (ENeg (ENeg (EDisj exp (ENeg exp))))))
  , (EImpl
      (EImpl (ENeg (EDisj exp (ENeg exp))) (ENeg (ENeg exp)))
      (ENeg (ENeg (EDisj exp (ENeg exp)))))
  , (ENeg (ENeg (EDisj exp (ENeg exp))))
  , (EImpl (ENeg (ENeg (EDisj exp (ENeg exp)))) (EDisj exp (ENeg exp)))
  , (EDisj exp (ENeg exp)) ]
  

totalProof :: [Exp] -> [Exp] -> VarMap -> Exp -> [Exp]
totalProof [] _ varmap target =
  generateAllProof target varmap
totalProof (gamma@(EVar name) : gammas) basicGammas varmap target =
  let
    proofPos :: [Exp]
    proofPos = totalProof gammas (gamma : basicGammas) (Map.insert name True varmap) target

    proofNeg :: [Exp]
    proofNeg = totalProof gammas ((ENeg gamma) : basicGammas) (Map.insert name False varmap) target

  in
    (deduct [gamma] basicGammas proofPos) ++
    (deduct [(ENeg gamma)] basicGammas proofNeg) ++
    proveThirdExcluded gamma ++
    [ (EImpl
        (EImpl gamma target)
        (EImpl
          (EImpl (ENeg gamma) target)
          (EImpl (EDisj gamma (ENeg gamma)) target)))
    , (EImpl
        (EImpl (ENeg gamma) target)
        (EImpl (EDisj gamma (ENeg gamma)) target))
    , (EImpl (EDisj gamma (ENeg gamma)) target)
    , target ]
totalProof gammas _ _ target = 
  error $ "Invalid call of total proof, exp = " ++ show target ++ 
    ", gammas = " ++ show gammas
