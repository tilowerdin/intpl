-- |module that contains class and instances of Pretty
module Pretty
  ( Pretty (..)
  ) where

import Type
import Data.Char
import Data.List

-- |Instances of this class provide the functions pretty which creates a string of 
-- this object. By deafualt it ist used show.
class Pretty a where
  -- |takes a list of variable indiced and their original identifier as string
  -- and makes pretty printing
  prettyWithVars :: [(VarIndex, String)] -> a -> String
  -- |takes the object and returns a representation of the string that looks 
  -- nicely.
  pretty :: a -> String

  pretty = prettyWithVars []

instance Pretty Term where
  prettyWithVars []           (Var v)    = getVar v
  prettyWithVars ((i,s):inds) (Var v)
    | i == v    = s
    | otherwise = prettyWithVars inds (Var v)
  prettyWithVars _ (Comb s [])           = s
  prettyWithVars inds (Comb "." [a1,a2]) = prettyList (Comb "." [a1,a2])
   where
    prettyList :: Term -> String 
    prettyList (Comb "." [h, (Var i)])          = "[" ++ prettyWithVars inds 
                                                                        h ++ 
                                                  "|" ++ prettyWithVars inds 
                                                                        (Var i) ++ 
                                                  "]"
    prettyList (Comb "." [h, Comb "." [a1,a2]]) =
      "[" ++ prettyWithVars inds h ++ ", " ++ (tail . prettyList) (Comb "." [a1,a2])
    prettyList (Comb "." [h, Comb "[]" []])     = "[" ++ prettyWithVars inds h ++ "]"
    prettyList (Comb "." [h, l])                = "[" ++ prettyWithVars inds h ++
                                                  "|" ++ prettyWithVars inds l ++
                                                  "]"
  prettyWithVars inds (Comb s args)      = 
    s ++ "(" ++ intercalate ", " (map (prettyWithVars inds) args) ++ ")"
   

instance Pretty Subst where
  prettyWithVars _    (Subst []) = "{}"
  prettyWithVars inds (Subst s)  = "{" ++ intercalate ", " (map prettySubst s) ++"}"
   where 
    prettySubst :: (VarIndex, Term) -> String
    prettySubst (v, t) = prettyWithVars inds (Var v) ++ " -> " ++ 
                         prettyWithVars inds t

getVar :: VarIndex -> String
getVar i 
  | i < 26    = (toEnum (65 + i)) : []
  | otherwise = 
    let m = i `mod` 26
    in getVar m ++ getVar (((i - m) `div` 26) - 1)

testPretty :: String
testPretty = pretty (Comb "append" [ Var 0
                                   , Comb "." [ Var 1
                                              , Var 2
                                              ]
                                   , Comb "." [ Comb "1" []
                                              , Comb "." [ Comb "2" []
                                                         , Comb "[]" []
                                                         ]
                                              ]
                                   ])


instance Eq Term where
  (==) (Var i) (Var j)         = i == j
  (==) (Var _) _               = False
  (==) _       (Var _)         = False
  (==) (Comb f lf) (Comb g lg) = f == g && lf == lg

instance Pretty Goal where
  prettyWithVars inds (Goal ts) = intercalate ", " 
                                              (map (prettyWithVars inds) ts) ++ "."

instance Pretty SLDTree where
  prettyWithVars _ sldTree = pretty' sldTree 0 where
    pretty' (SLDTree goal pairs) n = 
      (take n (repeat ' ')) ++ "new Goal: " ++ pretty goal ++ "\n" ++ 
      concatMap (\ (sub, tree) -> (take (n+2) (repeat ' ')) ++ 
                                  "mgu: " ++ pretty sub ++ "\n" 
                                  ++ pretty' tree (n+2)) 
                pairs