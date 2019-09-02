module Compiler 
( compile
) where

import Parser
import SemanticAnalyzer

compile :: AnnotatedProgram -> String
compile (AnnotatedProgram exprs) = 
    foldl (\acc x -> acc ++ x ++ "\n") header (map generateCode exprs)
    where
        header = "global start\nsection .text\nstart:\n\
        \call main              ; Call the main function\n\
        \movq rdi, rax          ; Main's return value is our exit code\n\
        \movq rax, 0x2000001    ; Code for exit() syscall\n\
        \syscall                ; Make the syscall\n"

generateCode :: AnnotatedExpr -> String
generateCode (AnnotatedExpr exprs annotation) = ""
