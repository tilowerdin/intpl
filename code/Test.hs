import Type
import Unify
import Subst
import SLDTree hiding (changeVariableNames, replaceVars)

changeVariableNames :: Rule -> [Term] -> Rule
changeVariableNames (r :- rs) ts = 
    let vsot     = concatMap getVarsOfTerm ts
        (r',_)   = replaceVars r ([],vsot)
        (Just s) = unify r r'
    in (apply s r) :- map (apply s) rs

replaceVars :: Term -> ([(VarIndex,VarIndex)],[VarIndex]) -> (Term, ([(VarIndex,VarIndex)],[VarIndex]))
replaceVars (Var v)     vl = case get v (fst vl) of
                              Just v' -> ((Var v'),vl)
                              Nothing -> let next = getNextFree vl
                                         in (Var next, ((v,next):(fst vl),snd vl))
replaceVars (Comb s []) vl = (Comb s [],vl)
replaceVars (Comb s ts) vl = let (t,vl1)           = replaceVars (head ts) vl
                                 (Comb _ ts', vl2) = 
                                   replaceVars (Comb s (tail ts)) vl1
                             in (Comb s (t:ts'),vl2)

get :: VarIndex -> [(VarIndex, VarIndex)] -> Maybe VarIndex
get _ [] = Nothing
get i ((v1,v2):vs)
  | i == v1   = Just v2
  | otherwise = get i vs

getNextFree :: ([(VarIndex, VarIndex)],[VarIndex]) -> VarIndex
getNextFree = go 0
  where 
    go i (f,s)
      | i `elem` s || i `elem` map snd f || i `elem` map fst f = go (i+1) (f,s)
      | otherwise = i