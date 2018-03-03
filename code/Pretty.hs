-- |module that contains class and instances of Pretty
module Pretty
  ( Pretty (..)
  ) where

import Type
import Data.Char
import Data.List

-- |Instances of this class provide the functions pretty which creates a string of 
-- this object. By deafualt it ist used show.
class Show a => Pretty a where
  -- |takes the object and returns a representation of the string that looks 
  -- nicely.
  pretty :: a -> String
  pretty = show

instance Pretty Term where
  pretty (Var i)               = getVar i
  pretty (Comb s [])           = s
  pretty (Comb "." [a1,a2])    = prettyList (Comb "." [a1,a2])
   where
    prettyList :: Term -> String 
    prettyList (Comb "." [h, (Var i)]) = "[" ++ pretty h ++ 
                                         "|" ++ getVar i ++ 
                                         "]"
    prettyList (Comb "." [h, Comb "." [a1,a2]]) =
      "[" ++ pretty h ++ ", " ++ (tail . prettyList) (Comb "." [a1,a2])
    prettyList (Comb "." [h, Comb "[]" []]) = "[" ++ pretty h ++ "]"
    prettyList (Comb "." [h, l])            = "[" ++ pretty h ++
                                              "|" ++ pretty l ++
                                              "]"
  pretty (Comb s args)         = s ++ "(" ++ intercalate ", " (map pretty args) ++ 
                                      ")"
   

instance Pretty Subst where
  pretty (Subst []) = "{}"
  pretty (Subst s)  = "{" ++ intercalate ", " (map prettySubst s) ++"}"
   where 
    prettySubst :: (VarIndex, Term) -> String
    prettySubst (v, t) = getVar v ++ " -> " ++ pretty t

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
  pretty (Goal ts) = intercalate ", " (map pretty ts) ++ "."

instance Pretty SLDTree where
  pretty sldTree = pretty' sldTree 0 where
    pretty' (SLDTree goal pairs) n = 
      (take n (repeat ' ')) ++ "new Goal: " ++ pretty goal ++ "\n" ++ 
      concatMap (\ (sub, tree) -> (take (n+2) (repeat ' ')) ++ 
                                  "mgu: " ++ pretty sub ++ "\n" 
                                  ++ pretty' tree (n+2)) 
                pairs