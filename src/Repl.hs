 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Repl where 

import System.Console.Repline
import Control.Monad.State.Strict
import Prelude hiding (init)
import System.Directory
import System.IO

import Data.List (isPrefixOf, intercalate, sortBy)
import Data.Ord (comparing)

import Term (Term (..), parseProg, showprog, atomName, instantiateTerm, parseTerm, eval, varsTerm') 
import Tree (residualise, trans)


type Prog = ([Term], [(Term, [Term])])
type IState = Maybe Prog
type Repl a = HaskelineT (StateT IState IO) a

cmd :: String -> Repl () 
cmd input = liftIO $ print input 

opts :: [(String, [String] -> Repl ())]
opts = [
    ("load", load)
  , ("prog", prog)
  , ("eval", evaluate)
  , ("trans", translate)
  , ("quit", quit)
  , ("help", help)
  ]

load :: [String] -> Repl () 
load [name] = do
  fileExists <- liftIO $ doesFileExist name
  if fileExists 
  then do 
      file <- liftIO $ readFile name
      case parseProg file of 
        Left s -> 
          liftIO $ putStrLn $ "Could not parse term in file " ++ name  
        Right t -> do 
          liftIO $ putStrLn $ "Loading file: " ++ name 
          put (Just t)
  else liftIO $ putStrLn $ "No such file: " ++ name
load _ = 
  liftIO $ putStrLn "Please only give me one file name "

prog :: [String] -> Repl () 
prog _ = do 
  p <- get 
  (liftIO . putStrLn) (maybe "No program loaded" showprog p) 

evaluate :: [String] -> Repl () 
evaluate _ = do 
  p <- get
  case p of
    Nothing -> liftIO $ putStrLn "No program loaded"
    Just (ts,cs) -> 
      liftIO $ f (foldr varsTerm' [] ts) ts cs []
        where
        f [] ts cs env = 
          let ts' = map (instantiateTerm env) ts
              xs  = foldr varsTerm' [] ts'
              tss = eval (map Var xs) ts' cs
          in  case tss of
                  [] -> liftIO $ putStrLn "False"
                  _ -> do 
                    liftIO $ putStrLn "True"
                    g xs tss
                      where
                        g xs [] = return () 
                        g xs (ts:tss) = h xs ts
                          where
                            h [] [] = g xs tss
                            h (x:xs) (t:ts) = do liftIO $ putStrLn $ x++" = "++show t
                                                 h xs ts
        f (x:xs) ts cs env = do 
          liftIO $ putStr $ x ++ " = "
          liftIO $ hFlush stdout
          l <- getLine
          case parseTerm l of
            Left s -> do liftIO $ putStrLn $ "Could not parse term: " ++ show s
                         f (x:xs) ts cs env
            Right u -> if   u == Var x
                       then f xs ts cs env
                       else f xs ts cs ((x,u):env)

translate :: [String] -> Repl () 
translate _ = do 
  p <- get 
  case p of 
    Nothing -> liftIO $ putStrLn "No program loaded"
    Just (ts, cs) -> do 
      let (ts', cs') = residualise $ trans ts cs 
      let cs'' = sortBy (comparing $ atomName . fst) cs'
      liftIO $ putStrLn $ showprog (ts', cs'')

quit :: [String] -> Repl () 
quit _ = abort 

help :: [String] -> Repl () 
help _ = liftIO $ putStrLn helpMessage

helpMessage = "\n:load filename\t\tTo load the given filename\n"++
               ":prog\t\t\tTo print the current program\n"++
               ":eval\t\t\tTo evaluate the current goal\n"++
               ":trans\t\t\tTo transform the current program\n"++
               ":quit\t\t\tTo quit\n"++
               ":help\t\t\tTo print this message\n"


init :: Repl ()
init = 
  liftIO $ putStrLn ("Welcome!\n" ++ helpMessage)

-- Completion
comp :: Monad m => WordCompleter m
comp = listWordCompleter $ map (':' :) $ map fst opts 

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load", fileCompleter)
  ]

repl = flip evalStateT Nothing
     $ evalRepl (pure "LOG> ") cmd opts (Just ':') (Prefix (wordCompleter comp) defaultMatcher) init

main = repl

