-- |module to solve a Goal with given program by choosing search algorithm in
-- SLD Tree
module Strategy
  ( dfs,
    bfs,
    solve
  ) where

import Type
import Subst
import Unify
import Pretty
import SLDTree

-- Strategy = SLDTree -> [Subst]

-- |depth first search in sld tree
dfs :: Strategy
dfs (SLDTree (Goal []) _)  = [empty]
dfs (SLDTree _         []) = []
dfs (SLDTree (Goal ts) l)  =
  map (\(Subst s) -> let vs = filter (appears ts) $ map fst s
                     in Subst $ filter (\(v,_) -> v `elem` vs) s) $
      concatMap (\(s1,sldTree) -> map (flip compose s1) (dfs sldTree))
                l
 where
  appears :: [Term] -> VarIndex -> Bool
  appears ts i = foldl (||) False $ map (go i) ts
   
  go :: VarIndex -> Term -> Bool
  go i (Var v)     = v == i
  go i (Comb _ ts) = appears ts i

-- |breadth first search in sld tree
bfs :: Strategy
bfs t@(SLDTree (Goal ts) slds) = go [(concatMap getVarsOfTerm ts, 
                                      empty,
                                      t)]
 where
  go :: [([VarIndex],Subst,SLDTree)] -> [Subst]
  go [] = []
  go l = let s = concatMap step l
             ready = map getSubst $ 
                         filter (\(_,_,z) -> case z of
                                               (SLDTree (Goal []) _) -> True
                                               otherwise             -> False)
                                s
             notReady = filter (\(_,_,z) -> case z of
                                              (SLDTree (Goal []) _) -> False
                                              (SLDTree _ [])        -> False     
                                              otherwise             -> True)
                               s
         in ready ++ go notReady

  step :: ([VarIndex],Subst,SLDTree) -> [([VarIndex],Subst,SLDTree)]
  step (vs, s, SLDTree (Goal []) _) = [(vs, s, SLDTree (Goal []) [])]
  step (vs, s, SLDTree g [])        = [(vs, s, SLDTree g [])]
  step (vs, s, SLDTree g l)         = map (\(slds, sldt) -> 
                                            (vs, compose slds s, sldt)) 
                                          l

  getSubst :: ([VarIndex],Subst,SLDTree) -> Subst
  getSubst (vs, (Subst s), _) = Subst (filter (\(v,_) -> v `elem` vs) s)

  getVarsOfTerm :: Term -> [VarIndex]
  getVarsOfTerm (Var i)     = [i]
  getVarsOfTerm (Comb _ ts) = concatMap getVarsOfTerm ts

-- |create the sld tree given a program and a goal to proof and search in the 
-- sld tree with the search strategy also given
solve :: Strategy -> Prog -> Goal -> [Subst]
solve s = (s .) . sld 

exProg :: Prog
exProg = Prog [
              ((Comb "p" [Var 23, Var 25]) :- [(Comb "q" [Var 23, Var 24]), (Comb "p" [Var 24, Var 25])]),
              ((Comb "p" [Var 23, Var 23]) :- []),
              ((Comb "q" [Comb "a" [], Comb "b" []] :- []))
              ]

exGoal :: Goal
exGoal = Goal [Comb "p" [Var 18, Comb "b" []]]