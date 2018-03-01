import Type
import Unify
import Subst
import SLDTree hiding (replaceVars)
import Control.Monad.State


--replaceVars :: Term -> ([(VarIndex,VarIndex)],[VarIndex]) -> (Term, ([(VarIndex,VarIndex)],[VarIndex]))
--replaceVars (Var v)     vl = case get v (fst vl) of
--                              Just v' -> ((Var v'),vl)
--                              Nothing -> let next = getNextFree vl
--                                         in (Var next, ((v,next):(fst vl),snd vl))
--replaceVars (Comb s []) vl = (Comb s [],vl)
--replaceVars (Comb s ts) vl = let (t,vl1)           = replaceVars (head ts) vl
--                                 (Comb _ ts', vl2) = 
--                                   replaceVars (Comb s (tail ts)) vl1
--                             in (Comb s (t:ts'),vl2)

replaceVars :: Term -> [VarIndex] -> Term
replaceVars t vl = evalState (go t) ([],vl)
  where 
    go :: Term -> State ([(VarIndex,VarIndex)],[VarIndex]) Term
    go (Var v) = do
      (ss,vl) <- get
      case lookup v ss of
        Just v' -> return (Var v')
        Nothing -> do
          let next = getNext (ss,vl)
          put ((v,next):ss,vl)
          return (Var next)
    go c@(Comb s []) = return c
    go (Comb s (t:ts)) = do
      t' <- go t
      (Comb s ts') <- go (Comb s ts)
      return (Comb s (t':ts'))