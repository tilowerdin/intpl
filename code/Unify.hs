module Unify where

import Type
import Subst
import Data.Maybe

ds :: Term -> Term -> Maybe (Term, Term)
ds (Var v1) (Var v2)
  | v1 == v2  = Nothing
  | otherwise = Just (Var v1, Var v2)
ds (Var v1) t = Just (Var v1, t)
ds t (Var v2) = Just (Var v2, t)
ds t1@(Comb s1 l1) t2@(Comb s2 l2)
  | s1 /= s2  = Just (t1,t2)
  | otherwise = case length l1 /= length l2 of
                  True      -> Just (t1,t2)
                  otherwise -> compare l1 l2
  where
    compare []     []     = Nothing
    compare (x:xs) (y:ys) = let res = ds x y
                            in if isNothing res 
                               then compare xs ys
                               else res

unify :: Term -> Term -> Maybe Subst
unify t1 t2 = case ds t1 t2 of 
                Nothing         -> Just (Subst [])
                Just (Var v, t) -> let s = single v t
                                   in case unify (apply s t1) (apply s t2) of
                                        Just s'   -> Just (compose s' s)
                                        otherwise -> Nothing
                otherwise       -> Nothing