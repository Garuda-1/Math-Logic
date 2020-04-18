module Main where

import Parser
import Lex
import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set



isAx1 :: Exp -> Bool
isAx1 (EImpl a0 (EImpl b1 a1))
  | (a0 == a1) = True
  | otherwise = False
isAx1 _ = False

isAx2 :: Exp -> Bool
isAx2 (EImpl (EImpl a0 b0) (EImpl (EImpl a1 (EImpl b1 c1)) (EImpl a2 c2)))
  | a0 == a1 && a1 == a2 && b0 == b1 && c1 == c2 = True
  | otherwise = False
isAx2 _ = False

isAx3 :: Exp -> Bool
isAx3 (EImpl a0 (EImpl b0 (EConj a1 b1)))
  | a0 == a1 && b0 == b1 = True
  | otherwise = False
isAx3 _ = False

isAx4 :: Exp -> Bool
isAx4 (EImpl (EConj a0 b0) a1)
  | a0 == a1 = True
  | otherwise = False
isAx4 _ = False

isAx5 :: Exp -> Bool
isAx5 (EImpl (EConj a0 b0) b1)
  | b0 == b1 = True
  | otherwise = False
isAx5 _ = False

isAx6 :: Exp -> Bool
isAx6 (EImpl a0 (EDisj a1 b1))
  | a0 == a1 = True
  | otherwise = False
isAx6 _ = False

isAx7 :: Exp -> Bool
isAx7 (EImpl b0 (EDisj a1 b1))
  | b0 == b1 = True
  | otherwise = False
isAx7 _ = False

isAx8 :: Exp -> Bool
isAx8 (EImpl (EImpl a0 c0) (EImpl (EImpl b1 c1) (EImpl (EDisj a2 b2) c2)))
  | a0 == a2 && b1 == b2 && c0 == c1 && c1 == c2 = True
  | otherwise = False
isAx8 _ = False

isAx9 :: Exp -> Bool
isAx9 (EImpl (EImpl a0 b0) (EImpl (EImpl a1 (ENeg b1)) (ENeg a2)))
  | a0 == a1 && b0 == b1 && a1 == a2 = True
  | otherwise = False
isAx9 _ = False 

isAx10 :: Exp -> Bool
isAx10 (EImpl (ENeg (ENeg a0)) a1)
  | a0 == a1 = True
  | otherwise = False
isAx10 _ = False

isMP :: Exp -> Exp -> Exp -> Bool
isMP a0 (EImpl a1 b0) b1
  | a0 == a1 && b0 == b1 = True
  | otherwise = False
isMP _ _ _ = False



data Node = Node { getIndex :: Int, getExp :: Exp} deriving (Show, Ord)

instance Eq Node where
  (Node _ e1) == (Node _ e2) = e1 == e2

type DependencyMap = Map.Map Exp (Int, Int)
type ReorderMap = Map.Map Int Int

data Pack = Pack { 
  getPrev :: Map.Map Exp Int, 
  getMapImpl :: Map.Map Exp (Set.Set Node),
  getMapMP :: DependencyMap}
  
empty :: Pack
empty = Pack Map.empty Map.empty Map.empty

isLeaf :: Node -> [Exp] -> Bool
isLeaf (Node _ exp) assumptions
  = isAx1 exp 
  || isAx2 exp
  || isAx3 exp
  || isAx4 exp
  || isAx5 exp 
  || isAx6 exp
  || isAx7 exp
  || isAx8 exp
  || isAx9 exp
  || isAx10 exp
  || elem exp assumptions

buildDependencies :: [Node] -> [Exp] -> Maybe DependencyMap
buildDependencies [] _ = Just Map.empty
buildDependencies nodes assumptions =
  let 
    insertIfAbsent :: (Ord k) => k -> v -> Map.Map k v -> Map.Map k v
    insertIfAbsent key value map =
      case (Map.lookup key map) of
        Nothing -> Map.insert key value map
        _ -> map

    nextPackImpl :: Node -> Pack -> Pack
    nextPackImpl (Node index exp) (Pack prev mapImpl mapMP) =
      let
        newPrev = insertIfAbsent exp index prev
      in
        case exp of
          (EImpl a b) ->
            let
              newMapImpl = case (Map.lookup a mapImpl) of
                Just set -> Map.insert a (Set.insert (Node index exp) set) mapImpl
                Nothing -> Map.insert a (Set.singleton (Node index exp)) mapImpl
            in
              case (Map.lookup a prev) of
                Just pindex -> (Pack 
                  newPrev 
                  newMapImpl 
                  (insertIfAbsent b (index, pindex) mapMP))
                Nothing -> (Pack newPrev newMapImpl mapMP)
          _ -> (Pack newPrev mapImpl mapMP)
          
    nextPackExp :: Node -> Pack -> Pack
    nextPackExp (Node index exp) (Pack prev mapImpl mapMP) =
      case (Map.lookup exp mapImpl) of
        Just set ->
          let
            newMapMP = Set.foldr' 
              (\(Node pindex (EImpl exp texp)) s -> insertIfAbsent texp (pindex, index) s)
              mapMP set
          in
            Pack prev mapImpl newMapMP
        _ -> Pack prev mapImpl mapMP
        
    nextPack :: Node -> Pack -> Pack
    nextPack node pack = nextPackExp node $ nextPackImpl node pack          
    
    helper :: [Node] -> [Exp] -> Pack -> Maybe DependencyMap
    helper [] _ pack = Just (getMapMP pack)
    helper ((Node index exp) : nodes) assumptions pack = 
      let
        newPack = nextPack (Node index exp) pack
      in
        if (isLeaf (Node index exp) assumptions) then
          helper nodes assumptions newPack
        else
          case (Map.lookup exp (getMapMP newPack)) of
            Nothing -> Nothing
            Just (i1, i2) -> 
              if ((max i1 i2) < index) then 
                helper nodes assumptions newPack
              else
                Nothing
      
  in
    helper nodes assumptions empty
  
        

annotateSrc :: [Exp] -> [Node]
annotateSrc exps =
  let
    helper :: [Exp] -> Int -> [Node]
    helper [] _ = []
    helper (exp : exps) n = (Node n exp) : (helper exps (n + 1))
  in
    helper exps 1
    
 
splitOn :: String -> String -> [String]
splitOn s t =
  let
    helper :: String -> String -> String -> [String]
    helper s [] acc = [reverse acc]
    helper s t acc = 
      if (isPrefixOf s t) then
        (reverse acc) : (helper s (drop (length s) t) [])
      else 
        helper s (tail t) ((head t) : acc)
  in
    helper s t []
   
    
breakHeader :: String -> ([Exp], Exp)
breakHeader s =
  let
    [allAssumptions, target] = splitOn "|-" s
    assumptions = splitOn "," allAssumptions
    filteredAssumptions = filter (\s -> (not $ null s) || (not $ all isSpace s)) assumptions 
    assumptionsExp = map (calc . alexScanTokens) filteredAssumptions
    targetExp = calc $ alexScanTokens target
  in
    (assumptionsExp, targetExp)



readLinesList :: IO [String]
readLinesList = do
  s <- getLine
  if null s then
    return []
  else
    (s :) <$> readLinesList



pickNecessary :: Int -> [Node] -> [Exp] -> DependencyMap -> [Node]
pickNecessary _ [] _ _ = error "Little less but still epic"
pickNecessary index ((Node i exp) : nodes) assumptions deps =
  if (index == i) then
    if (isLeaf (Node i exp) assumptions) then
      [(Node i exp)]
    else
      case (Map.lookup exp deps) of
        Nothing -> [(Node i exp)]
        Just (i1, i2) -> 
          (Node i exp) : 
            ((pickNecessary i1 nodes assumptions deps) ++ 
            (pickNecessary i2 nodes assumptions deps))
  else
    pickNecessary index nodes assumptions deps
    
    

reorderingMaps :: [Node] -> ReorderMap
reorderingMaps nodes =
  let
    helper :: Int -> [Node] -> ReorderMap
    helper _ [] = Map.empty
    helper n ((Node i _) : nodes) =
      let
        old2new = helper (n + 1) nodes
      in
        Map.insert i n old2new
  in
    helper 1 nodes
    
    
    
toAnnotatedProof :: [Node] -> [Exp] -> DependencyMap -> ReorderMap -> [String]
toAnnotatedProof [] _ _ _ = []
toAnnotatedProof ((Node index exp) : nodes) assumptions deps old2new =
  let
    newIndex = case (Map.lookup index old2new) of
      Just i -> i
      Nothing -> error "New index not found"
    
    axString :: Int -> String
    axString i = "[" ++ (show newIndex) ++ ". Ax. sch. " ++ (show i) ++ "] "
      ++ (show exp)
    
    mpString :: Int -> Int -> String
    mpString i1 i2 = "[" ++ (show newIndex) ++ ". M.P. " ++ (show i1) ++ ", "
      ++ (show i2) ++ "] " ++ (show exp)
      
    assumptionString :: Int -> String
    assumptionString i = "[" ++ (show newIndex) ++ ". Hypothesis " ++ (show i)
      ++ "] " ++ (show exp)
      
    
    helper :: String
    helper =
      if (isAx1 exp) then
        axString 1
      else if (isAx2 exp) then
        axString 2
      else if (isAx3 exp) then
        axString 3
      else if (isAx4 exp) then
        axString 4
      else if (isAx5 exp) then
        axString 5
      else if (isAx6 exp) then
        axString 6
      else if (isAx7 exp) then
        axString 7
      else if (isAx8 exp) then
        axString 8
      else if (isAx9 exp) then
        axString 9
      else if (isAx10 exp) then
        axString 10
      else
        case (elemIndex exp assumptions) of
          Just i -> assumptionString (i + 1)
          Nothing -> case (Map.lookup exp deps) of
            Just (i1, i2) -> case (Map.lookup i1 old2new) of
              Just ni1 -> case (Map.lookup i2 old2new) of
                Just ni2 -> mpString ni1 ni2
                Nothing -> error "M.P. dependency index not found"
              Nothing -> error "M.P. dependency index not found"   
            Nothing -> error "Epic fail"
  in
    helper : (toAnnotatedProof nodes assumptions deps old2new)
    


main = do
  headerStr <- getLine
  contents <- getContents
  let linesStr = lines $ contents
  let header = breakHeader headerStr
  let nodes = annotateSrc $ map (calc . alexScanTokens) linesStr
  let assumptions = fst header
  let target = snd header
  case (buildDependencies nodes (fst $ header)) of
    Nothing -> do
      putStrLn "Proof is incorrect"
    (Just deps) -> do
      if ((getExp $ last nodes) /= (snd header)) then
        putStrLn "Proof is incorrect"
      else do
        let minimized = nub $ sortOn (getIndex) $ pickNecessary (length nodes) (reverse nodes) assumptions deps
        let old2new = reorderingMaps minimized
        putStr $ intercalate ", " (map (show) assumptions)
        if (null assumptions) then
          putStr "|- "
        else
          putStr " |- "
        putStrLn (show target)
        mapM_ (putStrLn) $ toAnnotatedProof minimized assumptions deps old2new

