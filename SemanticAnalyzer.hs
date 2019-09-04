module SemanticAnalyzer 
( annotate
, AnnotatedProgram (..)
, AnnotatedExpr (..)
, ImmediateValue (..)
) where

import Data.Map (Map)
import qualified Data.Map as Map

import Parser

-- Notes

-- You cannot have multiple assignments for the same name
-- If you assign a value, then change it, use the most recent value

-- Abstract Syntax Tree classes
data AnnotatedProgram = AnnotatedProgram [AnnotatedExpr] deriving (Show)

data AnnotatedExpr = Empty 
    | IntLiteral Int
    | FloatLiteral Float
    | BoolLiteral Bool
    | CharLiteral Char
    | ListLiteral [AnnotatedExpr]
    | VarUse String Bool -- Annotated with isFunc
    | FuncCall AnnotatedExpr [AnnotatedExpr]
    | Assignment String AnnotatedExpr Bool -- Annotated with isFunc
    | Func [String] AnnotatedExpr
    | Conditional AnnotatedExpr AnnotatedExpr AnnotatedExpr
    deriving (Show)

class ImmediateValue a where
    immediate :: a -> String

instance ImmediateValue AnnotatedExpr where  
    immediate (SemanticAnalyzer.IntLiteral value) = show value
    immediate (SemanticAnalyzer.FloatLiteral value) = show value
    immediate (SemanticAnalyzer.BoolLiteral value) = show value
    immediate (SemanticAnalyzer.CharLiteral value) = show value
    -- immediate (SemanticAnalyzer.ListLiteral value) = show value
    -- immediate (SemanticAnalyzer.VarUse name _) = show value
    -- immediate (SemanticAnalyzer.FuncCall func params) = show value
    -- immediate (SemanticAnalyzer.Assignment name value _) = show value
    -- immediate (SemanticAnalyzer.Func args body) = show value
    -- immediate (SemanticAnalyzer.Conditional condExpr primaryExpr altExpr) = show value

-- type SymbolTable = Map String (Int, Expr)

annotate :: Program -> AnnotatedProgram
annotate (Program exprs) = AnnotatedProgram $ map annotateExpr exprs

annotateExpr :: Expr -> AnnotatedExpr
annotateExpr (Parser.Assignment name value) = 
    SemanticAnalyzer.Assignment name (annotateExpr value) (isFunc value)

annotateExpr (Parser.Func args body) = 
    SemanticAnalyzer.Func args (annotateExpr body)

-- No Annotations Necessary
annotateExpr (Parser.IntLiteral value) = SemanticAnalyzer.IntLiteral value

isFunc :: Expr -> Bool
isFunc (Parser.Func _ _) = True
isFunc _ = False