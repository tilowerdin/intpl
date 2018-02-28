module SLDTree where

import Type
import Unify
import Subst

sld :: Prog -> Goal -> SLDTree
sld p (Goal [])   = SLDTree (Goal []) []
sld p g@(Goal ts) = 
  let l = findAllDerivations p g
  in SLDTree g (map (\(s, tss) -> (,) s 
                                    $ sld p 
                                      $ Goal $ map (apply s) $ tss ++ (tail ts)) l)

findAllDerivations :: Prog -> Goal -> [(Subst, [Term])]
findAllDerivations (Prog [])     _           = []
findAllDerivations (Prog (r:rs)) g@(Goal ts) =
  case testRule (changeVariableNames r ts) (head ts) of 
    (Just s)  -> (s, map (apply s) (derivation r)) : 
                 findAllDerivations (Prog rs) g
    otherwise -> findAllDerivations (Prog rs) g

changeVariableNames :: Rule -> [Term] -> Rule
changeVariableNames (r :- rs) ts = let vsot     = concatMap getVarsOfTerm ts
                                       (r',_)   = replaceVars r vsot
                                       (Just s) = unify r r'
                                   in (apply s r) :- map (apply s) rs

getVarsOfTerm :: Term -> [VarIndex]
getVarsOfTerm (Var v)     = [v]
getVarsOfTerm (Comb _ ts) = concatMap getVarsOfTerm ts

replaceVars :: Term -> [VarIndex] -> (Term, [VarIndex])
replaceVars (Var v)     vl = case v `elem` vl of
                               True  -> replaceVars (Var (v+1)) vl
                               False -> ((Var v), v:vl)
replaceVars (Comb s ts) vl = let (t,vl1)           = replaceVars (head ts) vl
                                 (Comb _ ts', vl2) = 
                                   replaceVars (Comb s (tail ts)) vl1
                             in (Comb s (t:ts),vl2)

testRule :: Rule -> Term -> Maybe Subst
testRule (r :- _) t = unify t r

derivation :: Rule -> [Term]
derivation (_ :- d) = d

-- take first element of Goal
-- search for all rules that can be applied which means unified
--   found one apply it and go recursiv deeper

testGoal :: Goal
testGoal = (Goal [ Comb "=" [ Var 0,
                              Comb "4" []
                            ],
                   Comb "=" [ Var 1,
                              Comb "hallo" []
                            ]
                 ])

testSLDTree :: Goal -> SLDTree
testSLDTree g = let p = Prog [ (Comb "=" [Var 0,Var 0]) :- []
                             ]
                in sld p g