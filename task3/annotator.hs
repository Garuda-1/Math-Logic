module Main where

import Parser
import Lex
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set



isVarFree :: Exp -> String -> Bool
isVarFree (EImpl x y) v = (isVarFree x v) || (isVarFree y v)
isVarFree (EDisj x y) v = (isVarFree x v) || (isVarFree y v)
isVarFree (EConj x y) v = (isVarFree x v) || (isVarFree y v)
isVarFree (ENeg x) v = isVarFree x v
isVarFree (EForall (EVar v1) x) v2 = if (v1 == v2) then False else isVarFree x v2
isVarFree (EForany (EVar v1) x) v2 = if (v1 == v2) then False else isVarFree x v2
isVarFree (EVar v1) v2 = v1 == v2
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
gatherFreeVars (EPred x) s = 
  Set.empty
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



gatherThetas :: String -> Exp -> Exp -> Set.Set String -> Maybe (Set.Set Exp)
gatherThetas s (EImpl x1 y1) (EImpl x2 y2) locked =
  case (gatherThetas s x1 x2 locked) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2 locked) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s (EDisj x1 y1) (EDisj x2 y2) locked =
  case (gatherThetas s x1 x2 locked) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2 locked) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s (EConj x1 y1) (EConj x2 y2) locked =
  case (gatherThetas s x1 x2 locked) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2 locked) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s (ENeg x1) (ENeg x2) locked =
  case (gatherThetas s x1 x2 locked) of
    Nothing -> Nothing
    Just thetas -> Just thetas
gatherThetas s (EForall (EVar s1) x1) (EForall (EVar s2) x2) locked =
  if (s1 /= s2) then
    Nothing
  else
    case (gatherThetas s x1 x2 (Set.insert s1 locked)) of
      Nothing -> Nothing
      Just thetas -> Just thetas
gatherThetas s (EForany (EVar s1) x1) (EForany (EVar s2) x2) locked =
  if (s1 /= s2) then
    Nothing
  else
    case (gatherThetas s x1 x2 (Set.insert s1 locked)) of
      Nothing -> Nothing
      Just thetas -> Just thetas
gatherThetas s (EVar s1) e locked =
  if (not (Set.member s1 locked) && s == s1) then 
    Just (Set.singleton e) 
  else 
    case e of
      (EVar s2) -> if (s1 == s2) then Just Set.empty else Nothing
      _ -> Nothing
gatherThetas s (EPred x1) (EPred x2) locked =
  Just Set.empty
gatherThetas s (EEq x1 y1) (EEq x2 y2) locked =
  case (gatherThetas s x1 x2 locked) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2 locked) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s (EAdd x1 y1) (EAdd x2 y2) locked =
  case (gatherThetas s x1 x2 locked) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2 locked) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s (EMul x1 y1) (EMul x2 y2) locked =
  case (gatherThetas s x1 x2 locked) of
    Nothing -> Nothing
    Just thetas1 -> case (gatherThetas s y1 y2 locked) of
      Nothing -> Nothing
      Just thetas2 -> Just (Set.union thetas1 thetas2)
gatherThetas s EZero EZero locked =
  Just Set.empty
gatherThetas s (EInc x1) (EInc x2) locked =
  case (gatherThetas s x1 x2 locked) of
    Nothing -> Nothing
    Just thetas -> Just thetas
gatherThetas s e1 e2 locked= 
  Nothing



isPredAx1 :: Exp -> Bool
isPredAx1 (EImpl a0 (EImpl b1 a1)) =
  (a0 == a1)
isPredAx1 _ = False

isPredAx2 :: Exp -> Bool
isPredAx2 (EImpl (EImpl a0 b0) (EImpl (EImpl a1 (EImpl b1 c1)) (EImpl a2 c2))) =
  a0 == a1 && a1 == a2 && b0 == b1 && c1 == c2
isPredAx2 _ = False

isPredAx3 :: Exp -> Bool
isPredAx3 (EImpl (EConj a0 b0) a1) =
  a0 == a1
isPredAx3 _ = False

isPredAx4 :: Exp -> Bool
isPredAx4 (EImpl (EConj a0 b0) b1) =
  b0 == b1
isPredAx4 _ = False

isPredAx5 :: Exp -> Bool
isPredAx5 (EImpl a0 (EImpl b0 (EConj a1 b1))) =
  a0 == a1 && b0 == b1
isPredAx5 _ = False

isPredAx6 :: Exp -> Bool
isPredAx6 (EImpl a0 (EDisj a1 b1)) =
  a0 == a1
isPredAx6 _ = False

isPredAx7 :: Exp -> Bool
isPredAx7 (EImpl b0 (EDisj a1 b1)) =
  b0 == b1
isPredAx7 _ = False

isPredAx8 :: Exp -> Bool
isPredAx8 (EImpl (EImpl a0 c0) (EImpl (EImpl b1 c1) (EImpl (EDisj a2 b2) c2))) =
  a0 == a2 && b1 == b2 && c0 == c1 && c1 == c2
isPredAx8 _ = False

isPredAx9 :: Exp -> Bool
isPredAx9 (EImpl (EImpl a0 b0) (EImpl (EImpl a1 (ENeg b1)) (ENeg a2))) =
  a0 == a1 && b0 == b1 && a1 == a2
isPredAx9 _ = False 

isPredAx10 :: Exp -> Bool
isPredAx10 (EImpl (ENeg (ENeg a0)) a1) =
  a0 == a1
isPredAx10 _ = False

isPredAx11 :: Exp -> Either String Bool
isPredAx11 (EImpl (EForall (EVar v) x) y) =
  do
    let maybeThetas = gatherThetas v x y Set.empty
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
    let maybeThetas = gatherThetas v y x Set.empty
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
  if (isVarFree a3 v) then
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
    if (isVarFree a0 v) then
      Left ("variable " ++ v ++ " occurs free in ?@-rule.")
    else
      Right True
  | otherwise = Right False
isForallInjection _ _ = Right False

isForanyInjection :: Exp -> Exp -> Either String Bool
isForanyInjection (EImpl a0 b0) (EImpl (EForany (EVar v) a1) b1)
  | a0 == a1 && b0 == b1 = 
    if (isVarFree b0 v) then
      Left ("variable " ++ v ++ " occurs free in ?@-rule.")
    else
      Right True
  | otherwise = Right False
isForanyInjection _ _ = Right False



data Node = Node { getIndex :: Int, getExp :: Exp} deriving (Show)

instance Eq Node where
  (Node _ e1) == (Node _ e2) = e1 == e2

instance Ord Node where
  (Node i1 e1) < (Node i2 e2) = 
    if (i1 < i2) then 
      True
    else if (i1 > i2) then
      False
    else 
      e1 < e2
  (Node i1 e1) <= (Node i2 e2) = 
    if (i1 < i2) then
      True
    else if (i1 > i2) then
      True
    else e1 <= e2

null :: Node
null = Node (-1) EZero

data Pack = Pack { 
  getPrev :: Map.Map Exp Int, 
  getMapImpl :: Map.Map Exp (Set.Set Node)}

empty :: Pack
empty = Pack Map.empty Map.empty

annotate :: [Node] -> Either [String] [String]
annotate [] = Right []
annotate nodes =
  let 

    nextPackImpl :: Node -> Pack -> Pack
    nextPackImpl (Node index exp) (Pack prev mapImpl) =
      let
        newPrev = Map.insert exp index prev
      in
        case exp of
          (EImpl a b) ->
            let
              newMapImpl = case (Map.lookup b mapImpl) of
                Just set -> Map.insert b (Set.insert (Node index exp) set) mapImpl
                Nothing -> Map.insert b (Set.singleton (Node index exp)) mapImpl
            in
              Pack newPrev newMapImpl
          _ -> Pack newPrev mapImpl
        
    nextPack :: Node -> Pack -> Pack
    nextPack node pack = nextPackImpl node pack          
    
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
            Left err -> curLine1
            Right res ->
              if (res) then
                Right $ "[" ++ show index ++ ". Ax. sch. 11] " ++ show exp
              else curLine1
                
        curLine1 :: Either String String
        curLine1 =
          case (isPredAx12 exp) of
                Left err -> curLine2
                Right res ->
                  if (res) then
                    Right $ "[" ++ show index ++ ". Ax. sch. 12] " ++ show exp
                  else curLine2

        curLine2 :: Either String String
        curLine2 = 
          if (isArithmAx9 exp) then
            Right $ "[" ++ show index ++ ". Ax. sch. A9] " ++ show exp
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
          else case (lookupMP pack) of
            Just (i1, i2) -> 
              Right $ "[" ++ show index ++ ". M.P. " ++ show i1 ++
                ", " ++ show i2 ++ "] " ++ show exp
            Nothing -> checkForanyInjection

        lookupMP :: Pack -> Maybe (Int, Int)
        lookupMP (Pack mapPrev mapImpl) =
          let
            helper :: (Map.Map Exp Int) -> (Set.Set Node) -> Maybe (Int, Int)
            helper prev set =
              let
                update :: Maybe (Int, Int) -> Node -> Maybe (Int, Int)
                update acc (Node j2 exp) =
                  case (exp) of
                    (EImpl a b) ->
                      case (Map.lookup a prev) of
                        Nothing -> acc
                        Just i2 -> 
                          case acc of
                            Nothing -> Just (i2, j2)
                            Just (i1, j1) ->
                              if ((i2 > i1) || (i2 == i1) && (j2 > j1)) then
                                Just (i2, j2)
                              else
                                Just (i1, j1)
                    _ -> acc
              in
                Set.foldl' update Nothing set
          in
            case (Map.lookup exp mapImpl) of
              Nothing -> Nothing
              Just set -> helper mapPrev set
                              
        checkForanyInjection :: Either String String
        checkForanyInjection =  
          case (lookupForanyInjection) of
            Right (Node pindex _) -> 
              if (pindex == getIndex Main.null) then
                checkForallInjection
              else
                Right $ "[" ++ show index ++ ". ?@-intro " ++ show pindex ++ 
                  "] " ++ show exp
            Left _ -> checkForallInjection

        checkForallInjection :: Either String String
        checkForallInjection =  
          case (lookupForallInjection) of
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
              case (Map.lookup (EImpl x y) (getPrev pack)) of
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
              case (Map.lookup (EImpl x y) (getPrev pack)) of
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

proof2 :: Exp -> [Node] -> Either [String] [String] -> [String]
proof2 target nodes proof =
  case (proof) of
    Left lines ->
      lines
    Right lines ->
      if (Data.List.null nodes || (getExp (last nodes) /= target)) then do
        lines ++ ["The proof proves different expression."]
      else
        lines

main = do
  vdash <- getChar
  vdash <- getChar
  targetStr <- getLine 
  let target = (calc . alexScanTokens) targetStr

  contents <- getContents
  let linesStr = lines $ contents
  let nodes = createNodes $ map (calc . alexScanTokens) linesStr

  let proof = annotate nodes
  putStrLn $ "|-" ++ (show target) ++ "\n" ++ 
    (intercalate "\n" (proof2 target nodes proof))
