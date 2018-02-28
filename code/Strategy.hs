module Strategy where

import Type
import Subst
import Unify
import Pretty
import SLDTree

-- Strategy = SLDTree -> [Subst]

dfs :: Strategy
dfs (SLDTree (Goal []) _)  = [Subst []]
dfs (SLDTree _         []) = []
dfs (SLDTree (Goal ts) l)  =
  map (\(Subst s) -> let vs = filter (appears ts) $ map fst s
                     in Subst $ filter (\(v,_) -> v `elem` vs) s) $
      concatMap (\(s1,sld) -> case dfs sld of
                                [] -> []
                                ss -> map (flip compose s1) ss)  
                l

appears :: [Term] -> VarIndex -> Bool
appears ts i = foldl (||) False $ map (go i) ts
  where 
    go i (Var v)     = v == i
    go i (Comb _ ts) = appears ts i


bfs :: Strategy
bfs t@(SLDTree (Goal ts) slds) = go [(concatMap getVarsOfTerm ts, 
                                      Subst [],
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

solve :: Strategy -> Prog -> Goal -> [Subst]
solve = undefined
