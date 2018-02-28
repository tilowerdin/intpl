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

-- returns whether | (True) or , (False) is required
prettyList :: Term -> (Symb, String) 
prettyList (Var i)           = (Line, "[" ++ pretty (Var i) ++ "]")
prettyList (Comb "." [h, l])
  | Line  == fst erg = (Komma, "[" ++ pretty h ++ "|" ++ tail (snd erg))
  | Komma == fst erg = (Komma, "[" ++ pretty h ++ ", " ++ tail (snd erg))
  | Not   == fst erg = (Komma, "[" ++ pretty h ++ "]")
  where 
    erg = prettyList l
prettyList (Comb "[]" [])    = (Not, "")
prettyList c                 = (Komma, pretty c)

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