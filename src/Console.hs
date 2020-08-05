module Console where

import Term
import Tree
import System.Directory

main :: IO ()
main = do
  mapM_ (test "examples")
        [ "doubleApp.pl"
        , "maxLengtho.pl"
        , "originalFirstNando.pl"
        , "originalFirstPlain.pl"
        , "originalLastNando.pl"
        , "originalLastPlain.pl"
        , "path.pl"
        , "unify.pl"
        , "typechecker.pl"
        -- , "typecheckerPeter.pl"
        ]

test directory fileName = do
  let name = directory ++ "/" ++ fileName
  fileExists <- doesFileExist name
  if fileExists
    then do
      file <- readFile name
      case parseProg file of
        Left s ->
          putStrLn $ "Could not parse term in file " ++ name
        Right t ->
          writeFile (directory ++ "/geoff_" ++ fileName) (showprog $ trans t)
    else putStrLn $ "No such file: " ++ name