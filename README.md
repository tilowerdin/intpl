# intpl
Project from CAU (Fortgeschrittene Programmierung). Creating an interactive prolog interpreter using haskell.

## Excercise 1 - Pretty Printing

Given is the module `Type.hs`. 
This module is going to be extended during the project. 
In the beginning there are the types `VarIndex`, `Term`, `Rule`, `Prog`, `Goal`.

We are defining a class `Pretty` that has the function `pretty -> a -> String` that constructs a pretty String out of a data type.
The first instance of `Pretty` is `Term`.

Instances and Methods that calculate pretty Strings are in the module `Pretty.hs`

## Excercise 2 - Substitution

We define the data type `Subst` which represents Substitutions and goes into the module `Type.hs`. For the type `Subst` it also exists an instance of `Pretty` in the `Pretty.hs` module. 

We create a new module named `Subst.hs` that contains the following functions:
* `empty :: Subst` to return an empty Substitution
* `single :: VarIndex -> Term -> Subst` which creates a substitution that substitutes the given variable in the first argument `VarIndex` with the term given in the second argument `Term`
* `apply :: Subst -> Term -> Term` which applies a given substitution to a given term and returns the term where all the variables of the substitution are replaced
* `compose :: Subst -> Subst -> Subst` which takes two substitutions and combines them


