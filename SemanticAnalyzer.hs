module SemanticAnalyzer 
( annotate
, AnnotatedProgram (..)
, AnnotatedExpr (..)
, Annotation (..)
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Parser

-- Abstract Syntax Tree classes
data AnnotatedProgram = AnnotatedProgram [AnnotatedExpr] deriving (Show)

data AnnotatedExpr = AnnotatedExpr Expr Annotation deriving (Show)

type Annotation = Map String String


annotate :: Program -> AnnotatedProgram
annotate (Program exprs) = AnnotatedProgram []