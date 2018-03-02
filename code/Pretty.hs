module Pretty where

import Type
import Data.Char

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  pretty (Var i)               = getVar i
  pretty (Comb s [])           = s
  pretty (Comb "." args)       = snd $ prettyList (Comb "." args)
  pretty (Comb s args)         = s ++ "(" ++ 
                                        pretty (head args) ++
                                        concatMap ((", "++) . pretty) (tail args) ++ 
                                      ")"

instance Pretty Subst where
  pretty (Subst []) = "{}"
  pretty (Subst s)  = "{" ++
                        prettySubst (head s) ++
                        concatMap ((", "++) . prettySubst) (tail s) ++
                      "}"
                      where 
                        prettySubst (v, t) = getVar v ++ " -> " ++ pretty t

data Symb = Line | Komma | Not
  deriving (Eq, Show)

prettyList :: Term -> (Symb, String) 
prettyList (Var i)           = (Line, "[" ++ pretty (Var i) ++ "]")
prettyList (Comb "." [h, l])
  | Line  == fst erg = (Komma, "[" ++ pretty h ++ "|" ++ tail (snd erg))
  | Komma == fst erg = (Komma, "[" ++ pretty h ++ ", " ++ tail (snd erg))
  | Not   == fst erg = (Komma, "[" ++ pretty h ++ "]")
  where 
    erg = prettyList l
prettyList (Comb "[]" [])    = (Not, "")
prettyList t                 = error(pretty t ++ " is not a list constructor")

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
  pretty (Goal terms) = concatMap (\t -> pretty t ++ if t == (last terms) then "." else ", ") terms

instance Pretty SLDTree where
  pretty sldTree = pretty' sldTree 0 where
    pretty' (SLDTree goal pairs) n = "new Goal: " ++ pretty goal ++ "\n" ++ concatMap (\ (sub, tree) -> (take n) (repeat ' ') ++ "mgu: " ++ pretty sub ++ " " ++ pretty' tree (n+2) ) pairs