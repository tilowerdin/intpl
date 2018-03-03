-- |module to create an SLD Tree of a goal to proof.
module SLDTree
  ( sld
  ) where

import Type
import Unify
import Subst
import Pretty
import Data.Maybe
import Data.List

-- |creates an SLD Tree from a given program and a given goal to proof
sld :: Prog -> Goal -> SLDTree
sld p              (Goal [])   = SLDTree (Goal []) []
sld p@(Prog rules) g@(Goal ts) = SLDTree g derivations
 where
  derivations = catMaybes $ map getDerivation rules

  getDerivation :: Rule -> Maybe (Subst, SLDTree)
  getDerivation r = let (rH :- rB) = getDistinctRule r
                        s          = unify (head ts) rH
                    in case s of
                         Nothing -> Nothing
                         Just s  -> Just (s, sld p (Goal (map (apply s) 
                                                              (rB ++ (tail ts)))))

  getDistinctRule :: Rule -> Rule
  getDistinctRule (rH :- rB) = let varsOfRule = nub $ concatMap getVarsOfTerm (rH:rB)
                                   varsOfGoal = nub $ concatMap getVarsOfTerm ts
                                   inte = intersect varsOfGoal varsOfRule
                                   unio = union varsOfRule varsOfGoal
                                   subst = getSubst inte unio
                               in (apply subst rH) :- (map (apply subst) rB)

  getVarsOfTerm :: Term -> [VarIndex]
  getVarsOfTerm (Var i)     = [i]
  getVarsOfTerm (Comb _ ts) = concatMap getVarsOfTerm ts

  getSubst :: [VarIndex] -> [VarIndex] -> Subst
  getSubst []     _    = empty
  getSubst (x:xs) used = let newX = getUnused 0 used
                         in compose (single x (Var newX)) (getSubst xs (newX:used))

  getUnused :: VarIndex -> [VarIndex] -> VarIndex
  getUnused startIndex used
    | startIndex `elem` used = getUnused (startIndex+1) used
    | otherwise              = startIndex





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

exProg :: Prog
exProg = Prog [
              ((Comb "p" [Var 23, Var 25]) :- [(Comb "q" [Var 23, Var 24]), (Comb "p" [Var 24, Var 25])]),
              ((Comb "p" [Var 23, Var 23]) :- []),
              ((Comb "q" [Comb "a" [], Comb "b" []] :- []))
              ]

exGoal :: Goal
exGoal = Goal [Comb "p" [Var 18, Comb "b" []]]