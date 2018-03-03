-- |Main module to start the action
module Main where

import Data.String
import Type
import Strategy
import Data.List
import Parser
import Pretty

main :: IO ()
main = do
  putStrLn "Welcome to Simple Prolog!"
  putStrLn "Type \":help\" for help."
  loop dfs (Prog [])
 where
  loop :: Strategy -> Prog -> IO ()
  loop strat prog = do
    putStr "?- "
    input <- getLine
    case filter (/= "") (words input) of 
      [":help"]      -> do printHelp
                           loop strat prog
      [":quit"]      -> return ()
      [":set","dfs"] -> do
        putStrLn "switched to dfs strategy"
        loop dfs prog
      [":set","bfs"] -> do
        putStrLn "switched to bfs strategy"
        loop bfs prog
      [":info"]      -> do
        printAllPredicates prog
        loop strat prog
      [":load",fp]   -> do
        prog' <- load fp prog
        loop strat prog'
      other          -> do
        exe strat prog $ concat other
        loop strat prog
  
  printHelp :: IO ()
  printHelp = do
    putStrLn "Commands available from the prompt:"
    putStrLn "  <goal>        Solves/proves the specified goal."
    putStrLn "  :help         Shows theis help message."
    putStrLn "  :info         Shows all available predicates."
    putStrLn "  :load <file>  Loads the specified file."
    putStrLn "  :quit         Exits the interactive environment."
    putStrLn "  :set <strat>  Sets the specified search strategy"
    putStrLn "                where <strat> is one of 'dfs' or 'bfs'." 

  printAllPredicates :: Prog -> IO ()
  printAllPredicates (Prog rs) = putStrLn $ intercalate ",\n" 
                                          $ nub 
                                          $ map getPredicate rs

  getPredicate :: Rule -> String
  getPredicate (Comb f fs :- _) = f ++ "/" ++ (show $ length fs)

  load :: FilePath -> Prog -> IO Prog
  load fp p = do
    erg <- parseFile fp :: IO (Either String Prog)
    case erg of
      Left s   -> do putStrLn s
                     return p
      Right p' -> return p'

  exe :: Strategy -> Prog -> String -> IO ()
  exe strat prog s = case parse s :: Either String Goal of
                       Left s  -> putStrLn s
                       Right g -> printSol $ map pretty 
                                           $ solve strat prog g

  printSol :: [String] -> IO ()
  printSol []     = do putStrLn "Nothing"
                       return ()
  printSol [x]    = putStrLn x
  printSol (x:xs) = do
    putStr x
    input <- getLine
    case input of
      ";" -> printSol xs
      _   -> return ()
