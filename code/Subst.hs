module Subst where

import Type
import Pretty

empty :: Subst
empty = Subst []

single :: VarIndex -> Term -> Subst
single v t = Subst [(v,t)]

apply :: Subst -> Term -> Term
apply (Subst []) t  = t
apply (Subst ((v,t):subs)) (Var i)
  | i == v    = t
  | otherwise = apply (Subst subs) (Var i)
apply s (Comb c xs) = Comb c $ map (apply s) xs

compose :: Subst -> Subst -> Subst
compose (Subst s2) (Subst s1) = 
  Subst (map (\x -> (,) (fst x) (apply (Subst s2) (snd x))) s1 ++ listminus s2 s1)

listminus :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
listminus l []        = l
listminus l ((a,_):r) = 
  let l' = deleteFirst l a
  in listminus l' r

deleteFirst :: Eq a => [(a,b)] -> a -> [(a,b)]
deleteFirst [] _ = []
deleteFirst ((a,b):l) c
  | a == c    = l
  | otherwise = (a,b) : deleteFirst l c

testSubst = pretty (compose (single 1 (Var 2))
                            (single 0 (Comb "f" [Var 1, Comb "true" []])))