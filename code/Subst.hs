-- |module for substitutions. apply and compose substitions
module Subst
  ( -- *constructors
    empty,
    single,
    
    -- *applications
    apply,
    compose
  ) where

import Type
import Pretty

-- |creates an empty substitution
empty :: Subst
empty = Subst []

-- |creates a substitution that has only one variable exchange
single :: VarIndex    -- ^the variable index to be replaced 
       -> Term        -- ^the term the variable is replaced with
       -> Subst
single v t = Subst [(v,t)]

-- |applies a substitution to a given term and replaces all the variables in the
-- term with the terms given in the subtitution
apply :: Subst    -- ^the subtitution to apply
      -> Term     -- ^the term the substitution is applied to
      -> Term
apply (Subst []) t = t
apply (Subst ((v,t):subs)) (Var i)
  | i == v    = t
  | otherwise = apply (Subst subs) (Var i)
apply s (Comb c xs) = Comb c $ map (apply s) xs

-- |compose to substitutions with each other. the returned substitution has the 
-- following elements: let a -> b be a variable exchange of the first substitution
-- (s1) then the goal substitution contains: a -> apply s2 b, when s2 is the second
-- substitution. the goal substitution also contains all c -> d of s2 for all c 
-- that are not present in s1 on the left side.
compose :: Subst    -- ^substitution s2
        -> Subst    -- ^substitution s1
        -> Subst
compose (Subst s2) (Subst s1) = 
  Subst (map (\x -> (,) (fst x) (apply (Subst s2) (snd x))) s1 ++ listminus s2 s1)
 where
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