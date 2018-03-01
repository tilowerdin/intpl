module SLDTree where

import Type
import Unify
import Subst

sld :: Prog -> Goal -> SLDTree
sld p (Goal [])   = SLDTree (Goal []) []
sld p g@(Goal ts) = 
  let l = findAllDerivations p g
  in SLDTree g 
             (map (\(s, tss) -> (,) s $ 
                                    sld p $ 
                                        Goal $ map (apply s) $ 
                                                   tss ++ (tail ts)) 
                  l)

findAllDerivations :: Prog -> Goal -> [(Subst, [Term])]
findAllDerivations (Prog [])     _           = []
findAllDerivations (Prog (r:rs)) g@(Goal ts) =
  case testRule (changeVariableNames r ts) (head ts) of 
    (Just s)  -> (s, map (apply s) (derivation r)) : 
                 findAllDerivations (Prog rs) g
    otherwise -> findAllDerivations (Prog rs) g

changeVariableNames :: Rule -> [Term] -> Rule
changeVariableNames (r :- rs) ts = 
    let vsot     = concatMap getVarsOfTerm ts
        (r',_)   = replaceVars r ([],vsot)
        (Just s) = unify r r'
    in (apply s r) :- map (apply s) rs

replaceVars :: Term -> ([(VarIndex,VarIndex)],[VarIndex]) -> (Term, ([(VarIndex,VarIndex)],[VarIndex]))
replaceVars (Var v)     vl = case getSub v (fst vl) of
                              Just v' -> ((Var v'),vl)
                              Nothing -> let next = getNext vl
                                         in (Var next, ((v,next):(fst vl),snd vl))
replaceVars (Comb s []) vl = (Comb s [],vl)
replaceVars (Comb s ts) vl = let (t,vl1)           = replaceVars (head ts) vl
                                 (Comb _ ts', vl2) = 
                                   replaceVars (Comb s (tail ts)) vl1
                             in (Comb s (t:ts'),vl2)

getSub :: VarIndex -> [(VarIndex, VarIndex)] -> Maybe VarIndex
getSub _ [] = Nothing
getSub i ((v1,v2):vs)
  | i == v1   = Just v2
  | otherwise = getSub i vs

getNext :: ([(VarIndex, VarIndex)],[VarIndex]) -> VarIndex
getNext = go 0
  where 
    go i (f,s)
      | i `elem` s || i `elem` map snd f = go (i+1) (f,s)
      | otherwise = i

getVarsOfTerm :: Term -> [VarIndex]
getVarsOfTerm (Var v)     = [v]
getVarsOfTerm (Comb _ ts) = concatMap getVarsOfTerm ts


testRule :: Rule -> Term -> Maybe Subst
testRule (r :- _) t = unify t r

derivation :: Rule -> [Term]
derivation (_ :- d) = d

-- take first element of Goal
-- search for all rules that can be applied which means unified
--   found one apply it and go recursiv deeper

testGoal :: Goal
testGoal = (Goal [ Comb "=" [ Var 0,
                              Var 1
                            ],
                   Comb "=" [ Var 2,
                              Comb "hallo" []
                            ]
                 ])

testSLDTree :: Goal -> SLDTree
testSLDTree g = let p = Prog [ (Comb "=" [Var 0,Var 0]) :- [Comb "True" []],
                               (Comb "=" [Comb "4" [],Comb "4" []] :- []),
                               (Comb "True" [] :- [])
                             ]
                in sld p g