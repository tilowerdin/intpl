-- |This module conatains all the types that are used in the project.
module Type
  ( VarIndex,
    Term(..), 
    Rule(..), 
    Prog(..), 
    Goal(..), 
    Subst(..), 
    SLDTree(..),
    Strategy(..)
  ) where

-- | Alias type for variables
type VarIndex = Int

-- | Data type for terms
data Term = Var VarIndex | Comb String [Term]
  deriving Show

-- | Data type for program rules
data Rule = Term :- [Term]
  deriving Show

-- | Data type for programs
data Prog = Prog [Rule]
  deriving Show

-- | Data type for goals
data Goal = Goal [Term]
  deriving Show

-- | Data type for Substitutions
data Subst = Subst [(VarIndex, Term)]
  deriving Show

-- | Data type for SLD Trees
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving Show

-- | Data type for a strategy
type Strategy = SLDTree -> [Subst]
