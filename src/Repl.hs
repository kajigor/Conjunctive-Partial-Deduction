
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

import Term
import Tree


type Program = (Term,[(Term,Term)])
type IState = Maybe Program
type Repl a = HaskelineT (StateT IState IO) a

cmd :: String -> Repl ()
cmd input = liftIO $ print input

opts :: [(String, [String] -> Repl ())]
opts = [
    ("load", load)
  , ("prog", program)
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

program :: [String] -> Repl ()
program _ = do
  p <- get
  (liftIO . putStrLn) (maybe "No program loaded" showprog p)

evaluate :: [String] -> Repl ()
evaluate _ = do
  p <- get
  case p of
    Nothing -> liftIO $ putStrLn "No program loaded"

    Just (t,d) ->
        liftIO $ f (vars t) t [] d
      where
        f [] t e d = let  t' = walk e t
                          xs = vars t'
                          rs = eval t' d
                      in  case rs of
                            [] -> liftIO $ putStrLn "False"
                            _ -> do liftIO $ putStrLn "True"
                                    g xs rs
                                    where
                                      g xs [] = return ()
                                      g xs (r:rs) = h xs r
                                        where
                                          h [] [] = g xs rs
                                          h (x:xs) (t:ts) = do
                                            liftIO $ putStrLn $ x++" = "++show t
                                            h xs ts
        f (x:xs) t e d = do liftIO $ putStr $ x++" = "
                            liftIO $ hFlush stdout
                            l <-  getLine
                            case parseTerm l of
                                Left s -> do
                                  liftIO $ putStrLn $ "Could not parse term: "++ show s
                                  f (x:xs) t e d
                                Right u ->
                                  if   u == Var x
                                  then f xs t e d
                                  else f xs t ((x,u):e) d


translate :: [String] -> Repl ()
translate _ = do
  p <- get
  case p of
    Nothing -> liftIO $ putStrLn "No program loaded"
    Just (ts, cs) -> do
      let p = trans (ts, cs)
      liftIO $ putStrLn $ showprog p

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
     $ evalRepl (pure "REPL> ") cmd opts (Just ':') (Prefix (wordCompleter comp) defaultMatcher) init

main = repl

