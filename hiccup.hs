module Hiccup where

import Parser
import SemanticAnalyzer
import Compiler

main :: IO ()
main = do
    args <- getArgs
    case args of
        (file: []) -> run file emptyEnvironment
        otherwise -> error "hiccup takes exactly 1 argument!"
    
build :: String -> IO ()
build fileName = do
    contents <- readFile fileName
    let annotatedProgram = annotate $ parse contents
    writeFile $ compile annotatedProgram