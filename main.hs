module Main where
import System.Environment
import System.IO
import Data.Map (Map)

--import Parser
import Interpreter

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> repl emptyEnvironment
        (file: []) -> run file emptyEnvironment
    
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
    let (value, newEnv) = evalInEnvironment expression env
    putStrLn (show value)
    repl newEnv