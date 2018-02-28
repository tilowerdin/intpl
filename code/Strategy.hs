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
                                ss -> map (flip compose s1) ss)  l

appears :: [Term] -> VarIndex -> Bool
appears ts i = foldl (||) False $ map (go i) ts
  where 
    go i (Var v)     = v == i
    go i (Comb _ ts) = appears ts i


bfs :: Strategy
bfs = undefined

solve :: Strategy -> Prog -> Goal -> [Subst]
solve = undefined
