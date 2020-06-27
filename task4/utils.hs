module Utils where

import Parser
import Proofs
import Cheats
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Data.Ord (comparing)



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

    allSubsets :: [String] -> [[String]]
    allSubsets [] = [[]]
    allSubsets (name : names) = 
      let
        others = allSubsets names
      in
        others ++ (map ((:) name) others)

    allSubsetsSorted :: [[String]]
    allSubsetsSorted = sortBy (comparing length) (allSubsets vars)

    generateVarmaps :: [String] -> [String] -> [VarMap]
    generateVarmaps [] _ = [Map.empty]
    generateVarmaps (name : names) locked =
      let
        rest = generateVarmaps names locked
      in
        if (elem name locked) then
          map (Map.insert name mode) rest
        else
          (map (Map.insert name True) rest) 
          ++ (map (Map.insert name False) rest)

    pickLocked :: [[String]] -> [String]
    pickLocked [] = error "whoa"
    pickLocked (set : sets) =
      if (and $ map (\varmap -> evaluate varmap exp) (generateVarmaps vars set)) then
        set
      else
        pickLocked sets
  
{-
    tryToDeviate :: [String] -> VarMap -> Exp -> String -> Bool
    tryToDeviate [] varmap exp target = 
      ((evaluate (Map.insert target True varmap) exp) 
      /= (evaluate (Map.insert target False varmap) exp))
    tryToDeviate (name : names) varmap exp target =
      if (target == name) then
        tryToDeviate names varmap exp target
      else
        case (Map.lookup name varmap) of
          Nothing ->
            ((tryToDeviate names (Map.insert name True varmap) exp target)
            || (tryToDeviate names (Map.insert name False varmap) exp target))
          Just _ -> tryToDeviate names varmap exp target
  
    pickNecessary :: [String] -> VarMap -> [String]
    pickNecessary [] _ = []
    pickNecessary (name : names) varmap =
      if (tryToDeviate vars varmap exp name) then
        name : (pickNecessary names (Map.insert name mode varmap))
      else
        pickNecessary names varmap
-}
{-
    helper :: [String] -> [VarMap] -> Exp -> Maybe [String]
    helper [] _ _ = Just []
    helper (name : names) varmaps exp = 
      if ((and $ map (\varmap -> evaluate (Map.insert name True varmap) exp) varmaps) &&
        (and $ map (\varmap -> evaluate (Map.insert name False varmap) exp) varmaps)) then
        helper names ((map (Map.insert name False) varmaps) ++ varmaps) exp
      else if (and $ map (\varmap -> evaluate (Map.insert name mode varmap) exp) varmaps) then
        case (helper names varmaps exp) of
          Nothing -> Nothing
          Just list -> Just $ name : list
      else
        Nothing
-}
  in
    if (evaluate basicVarMap exp) then
      Just $ pickLocked allSubsetsSorted
    else
      Nothing
    

defineSolution :: Exp -> Maybe (Bool, [Exp])
defineSolution exp =
  case (necessaryVars (Set.toList $ getVarNames exp) True exp) of 
    Just names -> Just (True, map (\name -> (EVar name)) names)
    Nothing -> case (necessaryVars (Set.toList $ getVarNames exp) False (ENeg exp)) of
      Just names -> Just (False, map (\name -> (ENeg (EVar name))) names)
      Nothing -> Nothing


generateProof :: Exp -> VarMap -> (Bool, [Exp])
{-
generateProof (EImpl aa@a0 bb@(ENeg (ENeg a1))) varmap
  | a0 == a1 =
    let
      a = a0
      
      res :: (Bool, [Exp])
      res = generateProof a varmap
    in
      (fst res, (snd res) ++ cheatImplANegNegA a)
  | otherwise =
    let 
      res1 :: (Bool, [Exp])
      res1 = generateProof aa varmap

      res2 :: (Bool, [Exp])
      res2 = generateProof bb varmap
    in 
      ((not $ fst res1) || (fst res2), 
        ((snd res1) ++ (snd res2) ++ (proveImpl aa (fst res1) bb (fst res2))))
-}
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
        (True, [])
      else 
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
