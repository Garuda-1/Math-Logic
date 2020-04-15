module Main where

import Parser
import Lex
import Data.List
import Data.List.Split
import Data.Char



isAx1 :: Exp -> Bool
isAx1 (EImpl a0 (EImpl b a1))
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



data Node = Node { getIndex :: Int, getExp :: Exp} deriving (Show)
type DependencyMap = [(Int, (Int, Int))]
type ReorderMap = [(Int, Int)]

buildDependencies :: [Node] -> [Exp] -> Maybe DependencyMap
buildDependencies [] _ = Just []
buildDependencies ((Node index exp) : exps) assumptions =
  let
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

    lookupMP :: Node -> [Node] -> [(Node, Node)]
    lookupMP (Node index exp) l = [(p0, p1) | 
      p0 <- l,
      p1 <- l,
      getIndex p0 < getIndex p1, 
      getIndex p1 < index,
      isMP (getExp p0) (getExp p1) exp]
      
    addDependencies :: [(Node, Node)] -> Maybe DependencyMap -> Int -> Maybe DependencyMap
    addDependencies _ Nothing _ = Nothing
    addDependencies mps (Just deps) i =
      --Just $ foldr (\((Node i1 _), (Node i2 _)) s -> ((i1, i2), i) : s) deps mps
      let
        ((Node i1 _), (Node i2 _)) = head mps
      in
        Just ((i, (i1, i2)) : deps)
  in
    if (isLeaf (Node index exp) assumptions) then
      buildDependencies exps assumptions
    else
      case (lookupMP (Node index exp) exps) of
        [] -> Nothing
        mps -> addDependencies mps (buildDependencies exps assumptions) index
  
        

annotateSrc :: [Exp] -> [Node]
annotateSrc exps =
  let
    helper :: [Exp] -> Int -> [Node]
    helper [] _ = []
    helper (exp : exps) n = (Node n exp) : (helper exps (n + 1))
  in
    helper exps 1
    
    
    
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



pickNecessary :: Int -> [Node] -> DependencyMap -> [Node]
pickNecessary _ [] _ = []
pickNecessary index ((Node i exp) : nodes) deps =
  if (index == i) then
    case (lookup i deps) of
      Nothing -> [(Node i exp)]
      Just (i1, i2) -> 
        (Node i exp) : ((pickNecessary i1 nodes deps) ++ (pickNecessary i2 nodes deps))
  else
    pickNecessary index nodes deps
    
    

reorderingMaps :: [Node] -> (ReorderMap, ReorderMap)
reorderingMaps nodes =
  let
    helper :: Int -> [Node] -> (ReorderMap, ReorderMap)
    helper _ [] = ([], [])
    helper n ((Node i _) : nodes) =
      let
        (old2new, new2old) = helper (n + 1) nodes
      in
        ((i, n) : old2new, (n, i) : new2old)
  in
    helper 1 nodes
    
    
    
toAnnotatedProof :: [Node] -> DependencyMap -> ReorderMap -> ReorderMap -> [String]
toAnnotatedProof [] _ _ _ = []
toAnnotatedProof (Node index exp) deps old2new new2old =
  let
    newIndex = lookup index old2new
    
    axString :: Int -> String
    axString i = "[" ++ (show newIndex) ++ ". Ax. sch. " ++ i ++ "] " ++ (show exp)
    
    mpString :: Int -> Int -> String
    mpString i1 i2 = "[" ++ (show newIndex) ++ ". M.P. " ++ (show i1) ++ ", "
      ++ (show i2) ++ "] " ++ (show exp)
    
    helper :: String =
      case (lookup index deps) of
        Just (i1, i2) -> mpString i1 i2
        Nothing ->
          if (isAx1 exp) then
  in
    
    


main = do
  headerStr <- getLine
  linesStr <- readLinesList
  let header = breakHeader headerStr
  let nodes = reverse $ annotateSrc $ map (calc . alexScanTokens) linesStr
  case (buildDependencies nodes (fst $ header)) of
    Nothing -> do
      print "Proof is incorrect"
    (Just map) -> do
      if ((getExp $ head nodes) /= (snd header)) then
        print "Proof is incorrect"
      else do
        --print header
        --print lines
        --print $ show header
        --print $ show lines
        --print $ map
       print nodes
       print $ length nodes
       print $ lookup 8 map
       print $ sortOn (getIndex) $ pickNecessary (length nodes) (nodes) map

