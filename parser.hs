module Parser 
( parse
, Program (..)
, Expr (..)
) where

import Text.Read
import Data.Char (toLower)
import Control.Monad.State  

-- Lexer Tokens

data Token = INTLIT Int | FLOATLIT Float | BOOLLIT Bool | STRINGLIT String | CHARLIT Char | KEYWORD_ASSIGN
    | KEYWORD_LAMBDA | KEYWORD_IF | KEYWORD_THEN | KEYWORD_ELSE | VAR String | LPAREN | RPAREN | LBRACKET
    | RBRACKET | COMMA | EQUALS | SEMICOLON | DOLLAR 
    deriving (Show, Eq)

-- Language Grammar
data Program = Program [Expr] deriving (Show)

-- An expression evaluates to a value
data Expr = Empty 
    | IntLiteral Int
    | FloatLiteral Float
    | BoolLiteral Bool
    | CharLiteral Char
    | ListLiteral [Expr] -- Evaluates to a ListType object
    | VarUse String -- Evaluates to variable's value
    | FuncCall Expr [Expr] -- First argument must evaluate to VarUse
    | Assignment String Expr -- Evaluates to the expression on its right side
    | Func [String] Expr -- Evaluates to a FuncType object
    | Conditional Expr Expr Expr -- First argument must evaluate to a BoolType
    deriving (Show)

-- Turns a string into an expression tree.
parse :: String -> Program
parse str =
    if verify result
    then
        let (trees, _) = unzip result
        in
        Program trees
    else error $ "Check semicolons."
    where
        toks = tokenize str
        result = map (runState parseExpr) (splitBy toks SEMICOLON)

splitBy :: [Token] -> Token -> [[Token]]
splitBy str delim = wordsWhen (==delim) str

wordsWhen :: (Token -> Bool) -> [Token] -> [[Token]]
wordsWhen p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

verify :: [(Expr, [Token])] -> Bool
verify lst = (length (filter (\x -> (snd x) /= []) lst)) == 0

-- Lexer

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
    | x == '(' = LPAREN : (tokenize xs)
    | x == ')' = RPAREN : (tokenize xs)
    | x == '[' = LBRACKET : (tokenize xs)
    | x == ']' = RBRACKET : (tokenize xs)
    | x == ',' = COMMA : (tokenize xs)
    | x == ';' = SEMICOLON : (tokenize xs)
    | x == '\"' =
        let rest = dropWhile (/= '\"') xs
        in
            case rest of
                ('\"': xs2) -> STRINGLIT (takeWhile (/= '\"') xs) : (tokenize xs2)
                otherwise -> error $ "StringLiteral must be terminated by double quotes."
    | x == '\'' =
        case xs of
            (c:'\'':xs2) -> CHARLIT c : (tokenize xs2)
            otherwise -> error "CharLiteral must contain only one character and be terminated by single quote."
    | x == '=' =
        case xs of
            ('=': xs2) -> VAR "==" : (tokenize xs2)
            otherwise -> EQUALS: (tokenize xs)
    | x == '$' = DOLLAR : (tokenize xs)
    | isDigit x =
        if checkFloat (x:xs)
        then FLOATLIT (read ("0" ++ [x] ++ (takeWhile isFloat xs) ++ "0") :: Float) : (tokenize (dropWhile isFloat xs))
        else INTLIT (read ([x] ++ (takeWhile isDigit xs)) :: Int) : (tokenize (dropWhile isDigit xs))
    | isValidVarName x =
        let (name, left) = ([x] ++ (takeWhile isValidVarName xs), dropWhile isValidVarName xs)
        in
            case name of
                "assign" -> KEYWORD_ASSIGN : (tokenize left)
                "lambda" -> KEYWORD_LAMBDA : (tokenize left)
                "True" -> BOOLLIT True : (tokenize left)
                "False" -> BOOLLIT False : (tokenize left)
                "if" -> KEYWORD_IF : (tokenize left)
                "then" -> KEYWORD_THEN : (tokenize left)
                "else" -> KEYWORD_ELSE : (tokenize left)
                otherwise -> VAR name : (tokenize left)
    | x == ' ' || x == '\n' = tokenize xs
    | x == '#' = tokenize $ dropWhile (\c -> c /= '\n') xs
    | otherwise = error $ "Unknown character found: " ++ [x]

isDigit :: Char -> Bool
isDigit x = elem x "0123456789"

isFloat :: Char -> Bool
isFloat x = (x == '.') || isDigit x

checkFloat :: String -> Bool
checkFloat str =
    case (dropWhile isDigit str) of
        [] -> False
        (x:[]) -> x == '.'
        (x:xs) ->
            x == '.' && 
            case (dropWhile isDigit xs) of
                [] -> True
                (y:_) -> y /= '.'

isAlpha :: Char -> Bool
isAlpha x = (length $ filter (\c -> c == x) "ABCDEFGHIJKLMONPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") == 1

isBuiltIn :: Char -> Bool
isBuiltIn x = (length $ filter (\c -> c == x) "<>!=+-*/&|%") == 1

isValidVarName :: Char -> Bool
isValidVarName x = (isAlpha x) || (isDigit x) || (isBuiltIn x)

-- Parser

accept :: State [Token] Token
accept = state $
            \rem ->
                case rem of
                    (x:xs) -> (x, xs)
                    [] -> error $ "Unexpected EOF."

peek :: State [Token] Token
peek = state $
            \rem ->
                case rem of
                    [] -> error $ "Unexpected EOF."
                    otherwise -> (head rem, rem)

parseExpr :: State [Token] Expr
parseExpr = do
    nextToken <- accept
    case nextToken of
        LPAREN -> parseFuncCall
        LBRACKET -> parseListLiteral
        KEYWORD_ASSIGN -> parseAssign
        KEYWORD_IF -> parseConditional
        KEYWORD_LAMBDA -> parseLambda
        VAR name -> return $ VarUse name
        INTLIT val -> return $ IntLiteral val
        FLOATLIT val -> return $ FloatLiteral val
        BOOLLIT val -> return $ BoolLiteral val
        CHARLIT val -> return $ CharLiteral val
        STRINGLIT val -> return $ ListLiteral $ map CharLiteral val
        otherwise -> error $ "Expected expression, unexpected token: " ++ (show nextToken)

parseFuncCall :: State [Token] Expr
parseFuncCall = do
    funcVarUse <- parseExpr
    case funcVarUse of
        (VarUse funcName) -> parseFuncCallArguments funcVarUse
        (Func _ _) -> do
            nextToken <- accept
            case nextToken of
                RPAREN -> return $ funcVarUse
                otherwise -> error $ "Forgot closing parenthesis on lambda function."
        otherwise -> error $ "Expected function name or lambda, got " ++ (show funcVarUse)

parseFuncCallArguments :: Expr -> State [Token] Expr
parseFuncCallArguments funcVarUse = do
    nextToken <- peek
    case nextToken of
        RPAREN -> do
            accept
            return $ FuncCall funcVarUse []
        otherwise -> do
            nextArg <- parseExpr
            partialFuncCall <- parseFuncCallArguments funcVarUse
            let (FuncCall _ exprs) = partialFuncCall in
                return $ FuncCall funcVarUse (nextArg:exprs)

parseListLiteral :: State [Token] Expr
parseListLiteral = do
    nextToken <- peek
    case nextToken of
        RBRACKET -> do
            accept
            return $ ListLiteral []
        otherwise -> do
            element <- parseExpr
            nextToken2 <- accept
            case nextToken2 of
                COMMA -> do
                    rem <- parseListLiteral
                    let (ListLiteral exprList) = rem in
                        return $ ListLiteral (element:exprList)
                RBRACKET -> return $ ListLiteral (element:[])
                otherwise -> error $ "Expected ',' or ']', unexpected token: " ++ (show nextToken2)

parseAssign :: State [Token] Expr
parseAssign = do
    nextToken <- accept
    nextToken2 <- accept
    tree <- parseExpr
    case nextToken of
        VAR name ->
            case nextToken2 of
                EQUALS -> return $ Assignment name tree
                otherwise -> error $ "Expected '=' after variable name in assignment."
        otherwise -> error $ "Expected variable name after ASSIGN."

parseConditional :: State [Token] Expr
parseConditional = do
    condExpr <- parseExpr
    nextToken <- accept
    primaryExpr <- parseExpr
    nextToken2 <- accept
    altExpr <- parseExpr
    case nextToken of
        KEYWORD_THEN ->
            case nextToken2 of
                KEYWORD_ELSE -> return $ Conditional condExpr primaryExpr altExpr
                otherwise -> error $ "Expected ELSE keyword, got: " ++ (show nextToken2)
        otherwise -> error $ "Expected THEN keyword, got: " ++ (show nextToken)

parseLambda :: State [Token] Expr
parseLambda = do
    nextToken <- accept
    case nextToken of
        VAR param -> do
            subfunc <- parseLambda
            let (Func vars body) = subfunc in
                return $ Func (param:vars) body
        DOLLAR -> do
            body <- parseExpr
            return $ Func [] body
        otherwise -> error $ "Malformed function declaration, expected arguments or '$', found: " ++ (show nextToken)