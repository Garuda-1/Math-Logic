module Proofs where

import Parser
import Axioms
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set



proveDuplication :: Exp -> [Exp]
proveDuplication exp =
  [ (EImpl exp (EImpl exp exp))
  , (EImpl (EImpl exp (EImpl exp exp)) 
      (EImpl 
        (EImpl exp (EImpl (EImpl exp exp) (exp))) 
        (EImpl exp exp)))
  , (EImpl 
      (EImpl exp (EImpl (EImpl exp exp) (exp))) 
      (EImpl exp exp))
  , (EImpl exp (EImpl (EImpl exp exp) (exp)))
  , (EImpl exp exp) ]


proveCounterposition :: Exp -> [Exp]
proveCounterposition (EImpl a b) =
  let
    gamma :: [Exp]
    gamma = [(ENeg b), (EImpl a b)]

    subproof :: [Exp]
    subproof =
      [ (EImpl a b)
      , (EImpl
          (EImpl a b)
          (EImpl
            (EImpl a (ENeg b))
            (ENeg a)))
      , (EImpl
          (EImpl a (ENeg b))
          (ENeg a))
      , (ENeg b)
      , (EImpl (ENeg b) (EImpl a (ENeg b)))
      , (EImpl a (ENeg b))
      , (ENeg a) ]
  in
    deduct gamma [] subproof
proveCounterposition exp = 
  error $ "Invalid call of counterposition proof: " ++ show exp

{-
proveSyllogism :: Exp -> Exp -> [Exp]
proveSyllogism (EImpl a b) (EImpl b1 c)
  | (b == b1) =
    let
      gamma :: [Exp]
      gamma = [a, (EImpl b c), (EImpl a b)]

      subproof :: [Exp]
      subproof =
        [ a
        , (EImpl a b)
        , b
        , (EImpl b c)
        , c ]
    in
      deduct gamma [] subproof
-}

proveDoubleNeg :: Exp -> [Exp]
proveDoubleNeg exp =
  [ (EImpl exp (EImpl (ENeg exp) exp))
  , (EImpl (ENeg exp) exp) ] ++
  proveCounterposition (EImpl (ENeg exp) exp) ++
  [ (EImpl (ENeg exp) (ENeg (ENeg exp))) ] ++
  proveDuplication (ENeg exp) ++
  [ (EImpl
      (EImpl (ENeg exp) (ENeg exp))
      (EImpl
        (EImpl (ENeg exp) (ENeg (ENeg exp)))
        (ENeg (ENeg exp))))
  , (EImpl
      (EImpl (ENeg exp) (ENeg (ENeg exp)))
      (ENeg (ENeg exp)))
  , (ENeg (ENeg exp)) ]


data Pack = Pack { getPrev :: Set.Set Exp
                 , getMapImpl :: Map.Map Exp (Set.Set Exp) }

empty :: Pack
empty = (Pack Set.empty Map.empty)

deduct :: [Exp] -> [Exp] -> [Exp] -> [Exp]
deduct [] _ proof = proof
deduct (hypothesis : gamma) gamma1 proof =
  let
    updatePack :: Pack -> Exp -> Pack
    updatePack (Pack prev mapImpl) exp =
      let
        newPrev = Set.insert exp prev
      in
        case exp of
          (EImpl a b) ->
            (Pack newPrev 
              (Map.insertWith (Set.union) b (Set.singleton exp) mapImpl))
          _ -> (Pack newPrev mapImpl)
          
    isMP :: Pack -> Exp -> Maybe (Exp, Exp)
    isMP (Pack prev mapImpl) exp =
      let
        folder :: Maybe (Exp, Exp) -> Exp -> Maybe (Exp, Exp)
        folder acc exp =
          case (exp) of
            (EImpl a b) ->
              if (Set.member a prev) then
                Just (a, exp)
              else
                acc
            _ -> acc
      in
        case (Map.lookup exp mapImpl) of
          Nothing -> Nothing
          Just set -> Set.foldl' folder Nothing set

    deductSingle :: Pack -> Exp -> (Set.Set Exp) -> [Exp] -> [Exp]
    deductSingle _ _ _ [] = []
    deductSingle pack hypothesis gamma (exp : exps) =
      let
        curProof :: [Exp]
        curProof =
          if (isAx exp || Set.member exp gamma) then
            [ exp
            , (EImpl exp (EImpl hypothesis exp))
            , (EImpl hypothesis exp) ]
          else if (exp == hypothesis) then
            proveDuplication exp
          else case (isMP pack exp) of
            Just (a, b) ->
              [ (EImpl
                  (EImpl hypothesis a)
                  (EImpl
                    (EImpl hypothesis b)
                    (EImpl hypothesis exp)))
              , (EImpl
                  (EImpl hypothesis b)
                  (EImpl hypothesis exp))
              , (EImpl hypothesis exp) ]
            Nothing -> error $ "Deduction failed for expression " ++ 
              show (EImpl hypothesis exp)
      in
        curProof ++ (deductSingle (updatePack pack exp) hypothesis gamma exps)
  in
    deduct gamma gamma1 $ 
      deductSingle empty hypothesis (Set.fromList (gamma ++ gamma1)) proof


proveAnd :: Exp -> Bool -> Exp -> Bool -> [Exp]
proveAnd a av b bv 
  | av && bv =
    [ (EImpl a (EImpl b (EConj a b)))
    , (EImpl b (EConj a b))
    , (EConj a b) ]
  | av && (not bv) =
    [ (EImpl (EConj a b) b)
    , (EImpl (ENeg b) (EImpl (EConj a b) (ENeg b)))
    , (EImpl (EConj a b) (ENeg b))
    , (EImpl 
        (EImpl (EConj a b) b) 
        (EImpl (EImpl (EConj a b) (ENeg b)) (ENeg (EConj a b))))
    , (EImpl (EImpl (EConj a b) (ENeg b)) (ENeg (EConj a b)))
    , (ENeg (EConj a b)) ]
  | (not av) && bv =
    [ (EImpl (EConj a b) a)
    , (EImpl (ENeg a) (EImpl (EConj a b) (ENeg a)))
    , (EImpl (EConj a b) (ENeg a))
    , (EImpl 
        (EImpl (EConj a b) a) 
        (EImpl (EImpl (EConj a b) (ENeg a)) (ENeg (EConj a b))))
    , (EImpl (EImpl (EConj a b) (ENeg a)) (ENeg (EConj a b)))
    , (ENeg (EConj a b)) ]
  | (not av) && (not bv) =
    [ (EImpl (EConj a b) a)
    , (EImpl (ENeg a) (EImpl (EConj a b) (ENeg a)))
    , (EImpl (EConj a b) (ENeg a))
    , (EImpl
        (EImpl (EConj a b) a) 
        (EImpl (EImpl (EConj a b) (ENeg a)) (ENeg (EConj a b))))
    , (EImpl (EImpl (EConj a b) (ENeg a)) (ENeg (EConj a b)))
    , (ENeg (EConj a b)) ]


proveOr :: Exp -> Bool -> Exp -> Bool -> [Exp]
proveOr a av b bv
  | av && bv =
    [ (EImpl a (EDisj a b))
    , (EDisj a b) ]
  | av && (not bv) =
    [ (EImpl a (EDisj a b))
    , (EDisj a b) ]
  | (not av) && bv =
    [ (EImpl b (EDisj a b))
    , (EDisj a b) ]
  | (not av) && (not bv) =
    let 
      gamma :: [Exp]
      gamma = [a, (ENeg a)]
  
      subproof :: [Exp]
      subproof =
        [ a
        , (EImpl a (EImpl (ENeg b) a))
        , (EImpl (ENeg b) a)
        , (ENeg a)
        , (EImpl (ENeg a) (EImpl (ENeg b) (ENeg a)))
        , (EImpl (ENeg b) (ENeg a))
        , (EImpl
            (EImpl (ENeg b) a)
            (EImpl
              (EImpl (ENeg b) (ENeg a))
              (ENeg (ENeg b))))
        , (EImpl
            (EImpl (ENeg b) (ENeg a))
            (ENeg (ENeg b)))
        , (ENeg (ENeg b))
        , (EImpl (ENeg (ENeg b)) b)
        , b ]
    in
      deduct gamma [(ENeg b)] subproof ++
      [ (EImpl (ENeg a) (EImpl a b))
      , (EImpl a b) ] ++
      proveDuplication b ++
      [ (EImpl
          (EImpl a b)
          (EImpl
            (EImpl b b)
            (EImpl (EDisj a b) b)))
      , (EImpl
          (EImpl b b)
          (EImpl (EDisj a b) b))
      , (EImpl (EDisj a b) b)
      , (EImpl
          (ENeg b)
          (EImpl
            (EDisj a b)
            (ENeg b)))
      , (EImpl (EDisj a b) (ENeg b))
      , (EImpl 
          (EImpl (EDisj a b) b)
          (EImpl 
            (EImpl (EDisj a b) (ENeg b))
            (ENeg (EDisj a b))))
      , (EImpl 
            (EImpl (EDisj a b) (ENeg b))
            (ENeg (EDisj a b)))
      , (ENeg (EDisj a b)) ]
{-
    [ (EImpl (ENeg b) (EImpl (ENeg a) (ENeg b)))
    , (EImpl (ENeg a) (ENeg b))] ++
    proveCounterposition (EImpl (ENeg a) (ENeg b)) ++
    [ (EImpl (ENeg (ENeg b)) (ENeg (ENeg a))) ] ++
    proveSyllogism (EImpl (ENeg (ENeg b)) (ENeg (ENeg a))) 
      (EImpl (ENeg (ENeg a)) a) ++
    [ (EImpl
        (EImpl (ENeg (ENeg a)) a)
        (EImpl (ENeg (ENeg b)) a))
    , (EImpl (ENeg (ENeg a)) a)
    , (EImpl (ENeg (ENeg b)) a) ] ++
    deduct [b] [a] (b : proveDoubleNeg b) ++
    proveSyllogism (EImpl b (ENeg (ENeg b))) (EImpl (ENeg (ENeg b)) a) ++
    [ (EImpl
        (EImpl (ENeg (ENeg b)) a)
        (EImpl b a))
    , (EImpl b a)
    , (EImpl
        (EImpl a a)
        (EImpl
          (EImpl b a)
          (EImpl (EDisj a b) a))) ] ++
    proveDuplication a ++
    [ (EImpl
        (EImpl b a)
        (EImpl (EDisj a b) a))
    , (EImpl (EDisj a b) a)
    , (EImpl (ENeg a) (EImpl (EDisj a b) (ENeg a)))
    , (EImpl (EDisj a b) (ENeg a))
    , (EImpl
        (EImpl (EDisj a b) a)
        (EImpl
          (EImpl (EDisj a b) (ENeg a))
          (ENeg (EDisj a b))))
    , (EImpl
        (EImpl (EDisj a b) (ENeg a))
        (ENeg (EDisj a b)))
    , (ENeg (EDisj a b)) ]
   -}
        
proveImpl :: Exp -> Bool -> Exp -> Bool -> [Exp]
proveImpl a av b bv
  | av && bv =
    [ (EImpl b (EImpl a b))
    , (EImpl a b) ]
  | av && (not bv) =
    let 
      gamma :: [Exp]
      gamma = [(EImpl a b)]

      subproof :: [Exp]
      subproof = 
        (proveDuplication b) ++
        [ (EImpl a b)
        , (EImpl (EImpl a b) (EImpl (EImpl b b) (EImpl (EDisj a b) b)))
        , (EImpl (EImpl b b) (EImpl (EDisj a b) b))
        , (EImpl (EDisj a b) b)
        , a
        , (EImpl a (EDisj a b))
        , (EDisj a b)
        , b
        , (EImpl b (EImpl (EImpl a b) b))
        , (EImpl (EImpl a b) b)
        , (ENeg b)
        , (EImpl (ENeg b) (EImpl (EImpl a b) (ENeg b)))
        , (EImpl (EImpl a b) (ENeg b))
        , (EImpl 
            (EImpl (EImpl a b) b) 
            (EImpl 
              (EImpl (EImpl a b) (ENeg b)) 
              (ENeg (EImpl a b))))
        , (EImpl (EImpl (EImpl a b) (ENeg b)) (ENeg (EImpl a b)))
        , (ENeg (EImpl a b)) ]
    in
      deduct gamma [a, (ENeg b)] subproof ++
      (proveDuplication (EImpl a b)) ++
      [ (EImpl
          (EImpl (EImpl a b) (EImpl a b))
          (EImpl 
            (EImpl (EImpl a b) (ENeg (EImpl a b)))
            (ENeg (EImpl a b))))
      , (EImpl 
          (EImpl (EImpl a b) (ENeg (EImpl a b)))
          (ENeg (EImpl a b)))
      , (ENeg (EImpl a b)) ]
  | (not av) && bv =
    [ (EImpl b (EImpl a b))
    , (EImpl a b) ]
  | (not av) && (not bv) =
    let
      gamma :: [Exp]
      gamma = [a]

      subproof :: [Exp]
      subproof =
        [ a
        , (EImpl a (EImpl (ENeg b) a))
        , (EImpl (ENeg b) a)
        , (ENeg a)
        , (EImpl (ENeg a) (EImpl (ENeg b) (ENeg a)))
        , (EImpl (ENeg b) (ENeg a))
        , (EImpl
            (EImpl (ENeg b) a)
            (EImpl
              (EImpl (ENeg b) (ENeg a))
              (ENeg (ENeg b))))
        , (EImpl
            (EImpl (ENeg b) (ENeg a))
            (ENeg (ENeg b)))
        , (ENeg (ENeg b))
        , (EImpl (ENeg (ENeg b)) b)
        , b ]
    in
      deduct gamma [(ENeg a), (ENeg b)] subproof


proveNeg :: Exp -> Bool -> [Exp]
proveNeg a av
  | av =
    proveDoubleNeg a
  | (not av) =
    []
