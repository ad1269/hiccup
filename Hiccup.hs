module Main where
import System.Environment
import System.IO

import Parser
import SemanticAnalyzer
import Compiler

main :: IO ()
main = do
    args <- getArgs
    case args of
        (file: []) -> build file
        otherwise -> error "hiccup takes exactly 1 argument!"
    
build :: String -> IO ()
build fileName = do
    contents <- readFile fileName
    let annotatedProgram = annotate $ parse contents
    writeFile "out" (compile annotatedProgram)