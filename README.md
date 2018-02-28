# intpl
Project from CAU (Fortgeschrittene Programmierung). Creating an interactive prolog interpreter using haskell.

## Excercise 1 - Pretty Printing

Given is the module `Type.hs`. 
This module is going to be extended during the project. 
In the beginning there are the types `VarIndex`, `Term`, `Rule`, `Prog`, `Goal`.

We are defining a class `Pretty` that has the function `pretty -> a -> String` that constructs a pretty String out of a data type.
The first instance of `Pretty` is `Term`.

Instances and Methods that calculate pretty Strings are in the module `Pretty.hs`

