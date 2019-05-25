module Main where
import System.Environment
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

import Parser

main :: IO ()
main = do
    repl $ Map.fromList []

repl :: Map String LangType -> IO ()
repl env = do
    putStr ">>> "
    hFlush stdout
    expression <- getLine
    let (value, newEnv) = evalProg (parse $ tokenize expression) env
    putStrLn (show value)
    repl newEnv