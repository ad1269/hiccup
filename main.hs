module Main where
import System.Environment
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

import Parser

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl $ Map.fromList []
        (file: []) -> run file $ Map.fromList []
    
run :: String -> Map String LangType -> IO ()
run fileName env = do
    contents <- readFile fileName
    let (res, env) = eval contents
    putStrLn $ fileName ++ " loaded."
    repl env

repl :: Map String LangType -> IO ()
repl env = do
    putStr ">>> "
    hFlush stdout
    expression <- getLine
    let (value, newEnv) = evalProg (parse $ tokenize expression) env
    putStrLn (show value)
    repl newEnv