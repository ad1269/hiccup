module Compiler 
( compile
) where

import SemanticAnalyzer

compile :: AnnotatedProgram -> String
compile (AnnotatedProgram exprs) = 
    foldl (\acc x -> acc ++ x ++ "\n") header (map generateCode exprs)
    where
        header = "; nasm -f macho64 out.s && \
        \ld -macosx_version_min 10.7.0 -lSystem -o out out.o\n\n\
        \global start\nsection .text\nstart:\n\
        \call main              ; Call the main function\n\
        \mov rdi, rax           ; Return value of main() is our exit code\n\
        \mov rax, 0x2000001     ; Code for exit() syscall\n\
        \syscall                ; Make the syscall\n"

generateCode :: AnnotatedExpr -> String
generateCode (IntLiteral value) = "mov rax, " ++ (show value)

-- Function assignment
-- Place function address pointer in rax
-- Generate function body at the end of the file
generateCode (Assignment name value True) = "mov rax, " ++ name
    ++ "\n" ++ name ++ ":\n" ++ (generateCode value)

-- Variable assignment
-- Push value onto stack with offset
generateCode (Assignment name value False) = "mov rax, " ++ (immediate value)

generateCode (Func args body) = generateCode body ++ "\nret\n"
