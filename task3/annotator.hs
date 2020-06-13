module Main where

import Parser
import Lex
import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set



isVarFree :: Exp -> Exp -> Bool
isVarFree (EImpl x y) v = (isVarFree x v) || (isVarFree y v)
isVarFree (EDisj x y) v = (isVarFree x v) || (isVarFree y v)
isVarFree (EConj x y) v = (isVarFree x v) || (isVarFree y v)
isVarFree (ENeg x) v = isVarFree x v
isVarFree (EForall (EVar v1) x) (EVar v2) = if (v1 == v2) then False else isVarFree x (EVar v2)
isVarFree (EForany (EVar v1) x) (EVar v2) = if (v1 == v2) then False else isVarFree x (EVar v2)
isVarFree (EVar v1) (EVar v2) = v1 == v2
isVarFree (EPred x) v = False
isVarFree (EEq x y) v = (isVarFree x v) || (isVarFree y v)
isVarFree (EAdd x y) v = (isVarFree x v) || (isVarFree y v)
isVarFree (EMul x y) v = (isVarFree x v) || (isVarFree y v)
isVarFree (EZero) v = False
isVarFree (EInc x) v = isVarFree x v



gatherFreeVars :: Exp -> Set.Set String -> Set.Set String
gatherFreeVars (EImpl x y) s =
  Set.union (gatherFreeVars x s) (gatherFreeVars y s)
gatherFreeVars (EDisj x y) s =
  Set.union (gatherFreeVars x s) (gatherFreeVars y s)
gatherFreeVars (EConj x y) s =
  Set.union (gatherFreeVars x s) (gatherFreeVars y s)
gatherFreeVars (ENeg x) s =
  gatherFreeVars x s
gatherFreeVars (EForall (EVar v1) x) s =
  gatherFreeVars x (Set.insert v1 s)
gatherFreeVars (EForany (EVar v1) x) s =
  gatherFreeVars x (Set.insert v1 s) 
gatherFreeVars (EVar v1) s =
  if (Set.member v1 s) then Set.empty else Set.singleton v1
gatherFreeVars (EPred x) s = Set.empty
gatherFreeVars (EEq x y) s =
  Set.union (gatherFreeVars x s) (gatherFreeVars y s)
gatherFreeVars (EAdd x y) s =
  Set.union (gatherFreeVars x s) (gatherFreeVars y s)
gatherFreeVars (EMul x y) s =
  Set.union (gatherFreeVars x s) (gatherFreeVars y s)
gatherFreeVars (EZero) s =
  Set.empty
gatherFreeVars (EInc x) s =
  gatherFreeVars x s


-- LEFT -- error
substituteFree :: String -> Exp -> Exp -> Set.Set String -> Set.Set String -> 
  Either Exp Exp
substituteFree v (EImpl x y) theta free s =
  case (substituteFree v x theta free s) of
    Left errExp -> Left errExp
    Right xs -> case (substituteFree v y theta free s) of
      Left errExp -> Left errExp
      Right ys -> Right (EImpl xs ys)
substituteFree v (EDisj x y) theta free s =
  case (substituteFree v x theta free s) of
    Left errExp -> Left errExp
    Right xs -> case (substituteFree v y theta free s) of
      Left errExp -> Left errExp
      Right ys -> Right (EDisj xs ys)
substituteFree v (EConj x y) theta free s =
  case (substituteFree v x theta free s) of
    Left errExp -> Left errExp
    Right xs -> case (substituteFree v y theta free s) of
      Left errExp -> Left errExp
      Right ys -> Right (EConj xs ys)
substituteFree v (ENeg x) theta free s =
  case (substituteFree v x theta free s) of
    Left errExp -> Left errExp
    Right xs -> Right (ENeg xs)
substituteFree v (EForall (EVar v1) x) theta free s =
  case (substituteFree v x theta free (Set.insert v1 s)) of
    Left errExp -> Left errExp
    Right xs -> Right (EForall (EVar v1) xs)
substituteFree v (EForany (EVar v1) x) theta free s =
  case (substituteFree v x theta free (Set.insert v1 s)) of
    Left errExp -> Left errExp
    Right xs -> Right (EForany (EVar v1) xs)
substituteFree v (EVar v1) theta free s =
  if ((Set.member v1 s) || v /= v1) then 
    Right (EVar v1) 
  else if (Set.null $ Set.intersection free s) then
    Right theta
  else
    Left theta
substituteFree v (EPred x) theta free s =
  Right (EPred x)
substituteFree v (EEq x y) theta free s =
  case (substituteFree v x theta free s) of
    Left errExp -> Left errExp
    Right xs -> case (substituteFree v y theta free s) of
      Left errExp -> Left errExp
      Right ys -> Right (EEq xs ys)
substituteFree v (EAdd x y) theta free s =
  case (substituteFree v x theta free s) of
    Left errExp -> Left errExp
    Right xs -> case (substituteFree v y theta free s) of
      Left errExp -> Left errExp
      Right ys -> Right (EAdd xs ys)
substituteFree v (EMul x y) theta free s =
  case (substituteFree v x theta free s) of
    Left errExp -> Left errExp
    Right xs -> case (substituteFree v y theta free s) of
      Left errExp -> Left errExp
      Right ys -> Right (EMul xs ys)
substituteFree v (EZero) theta free s =
  Right EZero
substituteFree v (EInc x) theta free s =
  case (substituteFree v x theta free s) of
    Left errExp -> Left errExp
    Right xs -> Right (EInc xs)



gatherThetas :: String -> Exp -> Exp -> Maybe (Set.Set Exp)
gatherThetas s (EImpl x1 y1) (EImpl x2 y2) =
  case (gatherThetas s x1 x2) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s (EDisj x1 y1) (EDisj x2 y2) =
  case (gatherThetas s x1 x2) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s (EConj x1 y1) (EConj x2 y2) =
  case (gatherThetas s x1 x2) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s (ENeg x1) (ENeg x2) =
  case (gatherThetas s x1 x2) of
    Nothing -> Nothing
    Just thetas -> Just thetas
gatherThetas s (EForall (EVar s1) x1) (EForall v2 x2) =
  if (s1 == s) then 
    Just Set.empty 
  else 
    case (gatherThetas s x1 x2) of
      Nothing -> Nothing
      Just thetas -> Just thetas
gatherThetas s (EForany (EVar s1) x1) (EForany v2 x2) =
  if (s1 == s) then
    Just Set.empty
  else 
    case (gatherThetas s x1 x2) of
      Nothing -> Nothing
      Just thetas -> Just thetas
gatherThetas s (EVar s1) e =
  if (s == s1) then Just (Set.singleton e) else Just Set.empty
gatherThetas s (EPred x1) (EPred x2) =
  Just Set.empty
gatherThetas s (EEq x1 y1) (EEq x2 y2) =
  case (gatherThetas s x1 x2) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s (EAdd x1 y1) (EAdd x2 y2) =
  case (gatherThetas s x1 x2) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s (EMul x1 y1) (EMul x2 y2) =
  case (gatherThetas s x1 x2) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s EZero EZero =
  Just Set.empty
gatherThetas s (EInc x1) (EInc x2) =
  case (gatherThetas s x1 x2) of
    Nothing -> Nothing
    Just thetas -> Just thetas
gatherThetas s e1 e2 =
  Nothing



isPredAx1 :: Exp -> Bool
isPredAx1 (EImpl a0 (EImpl b1 a1))
  | (a0 == a1) = True
  | otherwise = False
isPredAx1 _ = False

isPredAx2 :: Exp -> Bool
isPredAx2 (EImpl (EImpl a0 b0) (EImpl (EImpl a1 (EImpl b1 c1)) (EImpl a2 c2)))
  | a0 == a1 && a1 == a2 && b0 == b1 && c1 == c2 = True
  | otherwise = False
isPredAx2 _ = False

isPredAx3 :: Exp -> Bool
isPredAx3 (EImpl (EConj a0 b0) a1)
  | a0 == a1 = True
  | otherwise = False
isPredAx3 _ = False

isPredAx4 :: Exp -> Bool
isPredAx4 (EImpl (EConj a0 b0) b1)
  | b0 == b1 = True
  | otherwise = False
isPredAx4 _ = False

isPredAx5 :: Exp -> Bool
isPredAx5 (EImpl a0 (EImpl b0 (EConj a1 b1)))
  | a0 == a1 && b0 == b1 = True
  | otherwise = False
isPredAx5 _ = False

isPredAx6 :: Exp -> Bool
isPredAx6 (EImpl a0 (EDisj a1 b1))
  | a0 == a1 = True
  | otherwise = False
isPredAx6 _ = False

isPredAx7 :: Exp -> Bool
isPredAx7 (EImpl b0 (EDisj a1 b1))
  | b0 == b1 = True
  | otherwise = False
isPredAx7 _ = False

isPredAx8 :: Exp -> Bool
isPredAx8 (EImpl (EImpl a0 c0) (EImpl (EImpl b1 c1) (EImpl (EDisj a2 b2) c2)))
  | a0 == a2 && b1 == b2 && c0 == c1 && c1 == c2 = True
  | otherwise = False
isPredAx8 _ = False

isPredAx9 :: Exp -> Bool
isPredAx9 (EImpl (EImpl a0 b0) (EImpl (EImpl a1 (ENeg b1)) (ENeg a2)))
  | a0 == a1 && b0 == b1 && a1 == a2 = True
  | otherwise = False
isPredAx9 _ = False 

isPredAx10 :: Exp -> Bool
isPredAx10 (EImpl (ENeg (ENeg a0)) a1)
  | a0 == a1 = True
  | otherwise = False
isPredAx10 _ = False

isPredAx11 :: Exp -> Either String Bool
isPredAx11 (EImpl (EForall (EVar v) x) y) =
  do
    let maybeThetas = gatherThetas v x y
    case (maybeThetas) of
      Nothing -> Right False
      Just thetas ->
        if (Set.null thetas) then
          Right True
        else if ((Set.size thetas) /= 1) then
          Right False
        else do
          let theta = Set.elemAt 0 thetas
          let free = gatherFreeVars theta Set.empty
          case (substituteFree v x theta free Set.empty) of
            Left errExp -> Left ("variable " ++ v ++ " is not free for term " 
              ++ show errExp ++ " in ?@-axiom.")
            Right exp -> Right (y == exp)
isPredAx11 _ = Right False

isPredAx12 :: Exp -> Either String Bool
isPredAx12 (EImpl x (EForany (EVar v) y)) =
  do
    let maybeThetas = gatherThetas v y x
    case (maybeThetas) of
      Nothing -> Right False
      Just thetas ->
        if (Set.null thetas) then
          Right True
        else if ((Set.size thetas) /= 1) then
          Right False
        else do
          let theta = Set.elemAt 0 thetas
          let free = gatherFreeVars theta Set.empty
          case (substituteFree v y theta free Set.empty) of
            Left errExp -> Left ("variable " ++ v ++ " is not free for term " 
              ++ show errExp ++ " in ?@-axiom.")
            Right exp -> Right (x == exp)
isPredAx12 _ = Right False

isArithmAx1 :: Exp -> Bool
isArithmAx1 (EImpl (EEq (EVar "a") (EVar "b")) (EImpl (EEq (EVar "a") (EVar "c")) (EEq (EVar "b") (EVar "c")))) = True
isArithmAx1 _ = False

isArithmAx2 :: Exp -> Bool
isArithmAx2 (EImpl (EEq (EVar "a") (EVar "b")) (EEq (EInc (EVar "a")) (EInc (EVar "b")))) = True
isArithmAx2 _ = False

isArithmAx3 :: Exp -> Bool
isArithmAx3 (EImpl (EEq (EInc (EVar "a")) (EInc (EVar "b"))) (EEq (EVar "a") (EVar "b"))) = True
isArithmAx3 _ = False

isArithmAx4 :: Exp -> Bool
isArithmAx4 (ENeg (EEq (EInc (EVar "a")) EZero)) = True
isArithmAx4 _ = False

isArithmAx5 :: Exp -> Bool
isArithmAx5 (EEq (EAdd (EVar "a") EZero) (EVar "a")) = True
isArithmAx5 _ = False

isArithmAx6 :: Exp -> Bool
isArithmAx6 (EEq (EAdd (EVar "a") (EInc (EVar "b"))) (EInc (EAdd (EVar "a") (EVar "b")))) = True
isArithmAx6 _ = False

isArithmAx7 :: Exp -> Bool
isArithmAx7 (EEq (EMul (EVar "a") EZero) (EZero)) = True
isArithmAx7 _ = False

isArithmAx8 :: Exp -> Bool
isArithmAx8 (EEq (EMul (EVar "a") (EInc (EVar "b"))) (EAdd (EMul (EVar "a") (EVar "b")) (EVar "a"))) = True
isArithmAx8 _ = False

isArithmAx9 :: Exp -> Bool
isArithmAx9 (EImpl (EConj (a0) (EForall (EVar v) (EImpl a1 a2))) (a3)) =
  do
    let free = gatherFreeVars a3 Set.empty
    if (Set.member v free) then
      case (substituteFree v a3 EZero Set.empty Set.empty) of
        Left _ -> False
        Right a0s -> case (substituteFree v a3 (EInc (EVar v)) (Set.singleton v) Set.empty) of
          Left _ -> False
          Right a2s -> (a0 == a0s) && (a1 == a3) && (a2 == a2s)
    else
      False
isArithmAx9 _ = False

isMP :: Exp -> Exp -> Exp -> Bool
isMP a0 (EImpl a1 b0) b1
  | a0 == a1 && b0 == b1 = True
  | otherwise = False
isMP _ _ _ = False

isForallInjection :: Exp -> Exp -> Either String Bool
isForallInjection (EImpl a0 b0) (EImpl a1 (EForall (EVar v) b1))
  | a0 == a1 && b0 == b1 = 
    if (Set.member v (gatherFreeVars a0 Set.empty)) then
      Left ("variable " ++ v ++ " occurs free in ?@-rule.")
    else
      Right True
  | otherwise = Right False
isForallInjection _ _ = Right False

isForanyInjection :: Exp -> Exp -> Either String Bool
isForanyInjection (EImpl a0 b0) (EImpl (EForany (EVar v) a1) b1)
  | a0 == a1 && b0 == b1 = 
    if (Set.member v (gatherFreeVars b0 Set.empty)) then
      Left ("variable " ++ v ++ " occurs free in ?@-rule.")
    else
      Right True
  | otherwise = Right False
isForanyInjection _ _ = Right False



data Node = Node { getIndex :: Int, getExp :: Exp} deriving (Show, Ord)

instance Eq Node where
  (Node _ e1) == (Node _ e2) = e1 == e2

null :: Node
null = Node (-1) EZero

type DependencyMap = Map.Map Exp (Int, Int)

data Pack = Pack { 
  getPrev :: Map.Map Exp Int, 
  getMapImpl :: Map.Map Exp (Set.Set Node),
  getMapMP :: DependencyMap}

empty :: Pack
empty = Pack Map.empty Map.empty Map.empty

annotate :: [Node] -> Either [String] [String]
annotate [] = Right []
annotate nodes =
  let 
    nextPackImpl :: Node -> Pack -> Pack
    nextPackImpl (Node index exp) (Pack prev mapImpl mapMP) =
      let
        newPrev = Map.insert exp index prev
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
                  (Map.insert b (index, pindex) mapMP))
                Nothing -> (Pack newPrev newMapImpl mapMP)
          _ -> (Pack newPrev mapImpl mapMP)
          
    nextPackExp :: Node -> Pack -> Pack
    nextPackExp (Node index exp) (Pack prev mapImpl mapMP) =
      case (Map.lookup exp mapImpl) of
        Just set ->
          let
            newMapMP = Set.foldr' 
              (\(Node pindex (EImpl exp texp)) s -> Map.insert texp (pindex, index) s)
              mapMP set
          in
            Pack prev mapImpl newMapMP
        _ -> Pack prev mapImpl mapMP
        
    nextPack :: Node -> Pack -> Pack
    nextPack node pack = nextPackExp node $ nextPackImpl node pack          
    
    helper :: [Node] -> Pack -> Either [String] [String]
    helper [] pack = Right []
    helper ((Node index exp) : nodes) pack = 
      let
        newPack = nextPack (Node index exp) pack
        nextLines = helper nodes newPack

        curLine :: Either String String
        curLine =
          if (isPredAx1 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. 1] " ++ show exp
          else if (isPredAx2 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. 2] " ++ show exp
          else if (isPredAx3 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. 3] " ++ show exp
          else if (isPredAx4 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. 4] " ++ show exp
          else if (isPredAx5 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. 5] " ++ show exp
          else if (isPredAx6 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. 6] " ++ show exp
          else if (isPredAx7 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. 7] " ++ show exp
          else if (isPredAx8 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. 8] " ++ show exp
          else if (isPredAx9 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. 9] " ++ show exp
          else if (isPredAx10 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. 10] " ++ show exp
          else case (isPredAx11 exp) of
            Left err -> Left findError
            Right res ->
              if (res) then
                Right $ "[" ++ show index ++ ". Ax. sch. 11] " ++ show exp
              else case (isPredAx12 exp) of
                Left err -> Left findError
                Right res ->
                  if (res) then
                    Right $ "[" ++ show index ++ ". Ax. sch. 12] " ++ show exp
                  else if (isArithmAx1 exp) then
                    Right $ "[" ++ show index ++ ". Ax. A1] " ++ show exp
                  else if (isArithmAx2 exp) then
                    Right $ "[" ++ show index ++ ". Ax. A2] " ++ show exp
                  else if (isArithmAx3 exp) then
                    Right $ "[" ++ show index ++ ". Ax. A3] " ++ show exp
                  else if (isArithmAx4 exp) then
                    Right $ "[" ++ show index ++ ". Ax. A4] " ++ show exp
                  else if (isArithmAx5 exp) then
                    Right $ "[" ++ show index ++ ". Ax. A5] " ++ show exp
                  else if (isArithmAx6 exp) then
                    Right $ "[" ++ show index ++ ". Ax. A6] " ++ show exp
                  else if (isArithmAx7 exp) then
                    Right $ "[" ++ show index ++ ". Ax. A7] " ++ show exp
                  else if (isArithmAx8 exp) then
                    Right $ "[" ++ show index ++ ". Ax. A8] " ++ show exp
                  else if (isArithmAx9 exp) then
                    Right $ "[" ++ show index ++ ". Ax. sch. A9] " ++ show exp
                  else case (Map.lookup exp (getMapMP newPack)) of
                    Just (i1, i2) -> 
                      Right $ "[" ++ show index ++ ". M.P. " ++ show i2 ++
                        ", " ++ show i1 ++ "] " ++ show exp
                    Nothing -> checkArithmAx11
                              
        checkArithmAx11 :: Either String String
        checkArithmAx11 =  
          case (lookupForallInjection) of
            Right (Node pindex _) -> 
              if (pindex == getIndex Main.null) then
                checkArithmAx12
              else
                Right $ "[" ++ show index ++ ". ?@-intro " ++ show pindex ++ 
                  "] " ++ show exp
            Left _ -> Left findError

        checkArithmAx12 :: Either String String
        checkArithmAx12 =  
          case (lookupForanyInjection) of
            Right (Node pindex _) -> 
              if (pindex == getIndex Main.null) then
                Left findError
              else
                Right $ "[" ++ show index ++ ". ?@-intro " ++ show pindex ++ 
                  "] " ++ show exp
            Left _ -> Left findError

        lookupForallInjection :: Either String Node
        lookupForallInjection =
          case (exp) of
            (EImpl x (EForall (EVar v) y)) ->
              case (Map.lookup (EImpl x y) (getPrev newPack)) of
                Just pindex -> 
                  case (isForallInjection (EImpl x y) exp) of
                    Left err -> Left err
                    Right res -> 
                      if (res) then
                        Right (Node pindex (EImpl x y))
                      else Right Main.null
                Nothing -> Right Main.null
            _ -> Right Main.null

        lookupForanyInjection :: Either String Node
        lookupForanyInjection =
          case (exp) of
            (EImpl (EForany (EVar v) x) y) ->
              case (Map.lookup (EImpl x y) (getPrev newPack)) of
                Just pindex -> 
                  case (isForanyInjection (EImpl x y) exp) of
                    Left err -> Left err
                    Right res -> 
                      if (res) then
                        Right (Node pindex (EImpl x y))
                      else Right Main.null
                Nothing -> Right Main.null
            _ -> Right Main.null
                  
        findError :: String
        findError =
          case (lookupForallInjection) of
            Left err -> "Expression " ++ show index ++ ": " ++ err
            Right _ -> case (lookupForanyInjection) of
              Left err -> "Expression " ++ show index ++ ": " ++ err
              Right _ -> case (isPredAx11 exp) of
                Left err -> "Expression " ++ show index ++ ": " ++ err
                Right _ -> case (isPredAx12 exp) of
                  Left err -> "Expression " ++ show index ++ ": " ++ err
                  Right _ -> "Expression " ++ show index ++ " is not proved."
            
      in
        case (curLine) of
          Left err -> Left (err : [])
          Right line -> case (nextLines) of
            Left lines -> Left (line : lines)
            Right lines -> Right (line : lines)
      
  in
    helper nodes empty

createNodes :: [Exp] -> [Node]
createNodes lines =
  let
    helper :: Int -> [Exp] -> [Node]
    helper _ [] = []
    helper i (line : lines) = (Node i line) : (helper (i + 1) lines)

  in
    helper 1 lines

main = do
  vdash <- getChar
  vdash <- getChar
  targetStr <- getLine 
  let target = (calc . alexScanTokens) targetStr

  contents <- getContents
  let linesStr = lines $ contents
  let nodes = createNodes $ map (calc . alexScanTokens) linesStr

  --mapM_ (putStrLn . show) nodes

  let proof = annotate nodes

  putStr "|-"
  putStrLn (show target)
  case (proof) of
    Left lines -> mapM_ (putStrLn) lines
    Right lines ->
      if (Data.List.null nodes || (getExp (last nodes) /= target)) then do
        mapM_ (putStrLn) lines
        putStrLn "The proof proves different expression."
      else
        mapM_ (putStrLn) lines

