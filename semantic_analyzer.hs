module SemanticAnalyzer 
( annotate
, AnnotatedProgram (..)
, AnnotatedExpr (..)
, Annotation (..)
) where

import Parser

-- Abstract Syntax Tree classes
data AnnotatedProgram = AnnotatedProgram [AnnotatedExpr] deriving (Show)

data AnnotatedExpr = AnnotatedExpr Expr Annotation 

type Annotation = Map String String


annotate :: Program -> AnnotatedProgram