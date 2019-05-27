module Parser 
( parse
, Program (..)
, Expr (..)
) where

import Text.Read
import Data.Char (toLower)

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
        result = map parseExpr (splitBy toks SEMICOLON)

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

splitBy :: [Token] -> Token -> [[Token]]
splitBy str delim = wordsWhen (==delim) str

wordsWhen :: (Token -> Bool) -> [Token] -> [[Token]]
wordsWhen p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

verify :: [(Expr, [Token])] -> Bool
verify lst = (length (filter (\x -> (snd x) /= []) lst)) == 0

parseExpr :: [Token] -> (Expr, [Token])
parseExpr [] = (Empty, [])
parseExpr str =
    case head str of
        LPAREN -> parseFuncCall (tail str)
        LBRACKET -> parseListLiteral (tail str)
        KEYWORD_ASSIGN -> parseAssign (tail str)
        KEYWORD_IF -> parseConditional (tail str)
        VAR _ -> parseVarUse str
        INTLIT _ -> parseIntLiteral str
        FLOATLIT _ -> parseFloatLiteral str
        BOOLLIT _ -> parseBoolLiteral str
        CHARLIT _ -> parseCharLiteral str
        STRINGLIT _ -> parseStringLiteral str
        KEYWORD_LAMBDA -> parseFunction (tail str)
        otherwise -> error $ "Expected expression, unexpected token: " ++ (show (head str))


parseListLiteral :: [Token] -> (Expr, [Token])
parseListLiteral str =
    case str of
        (RBRACKET:xs) -> (ListLiteral [], xs)
        otherwise ->
            let (first, rest) = parseExpr str
            in
                case rest of
                    (COMMA: rest2) ->
                        let (ListLiteral exprList, finalRest) = parseListLiteral rest2
                        in
                            (ListLiteral (first:exprList), finalRest)
                    (RBRACKET: rest2) -> (ListLiteral (first:[]), rest2)
                    otherwise -> error $ "Expected ',' or ']', unexpected token: " ++ (show (head rest))

parseConditional :: [Token] -> (Expr, [Token])
parseConditional str =
    let (condExpr, rest) = parseExpr str
    in
        case rest of
            (KEYWORD_THEN:_) -> 
                let (primaryExpr, rest2) = parseExpr (tail rest)
                in
                    case rest2 of
                        (KEYWORD_ELSE:_) ->
                            let (altExpr, rest3) = parseExpr (tail rest2)
                            in
                                (Conditional condExpr primaryExpr altExpr, rest3)
                        otherwise -> error $ "Expected ELSE keyword, got: " ++ (show (head rest2))
            otherwise -> error $ "Expected THEN keyword, got: " ++ (show (head rest))

parseFunction :: [Token] -> (Expr, [Token])
parseFunction str =
    case str of
        (VAR param: xs) ->
            let (Func vars body, rest) = parseFunction xs
            in
                (Func (param:vars) body, rest)
        (DOLLAR: xs) ->
            case (tail str) of
                [] -> (Func [] Empty, [])
                (x:xs2) ->
                    let (expr, rest) = parseExpr (x:xs2)
                    in
                        (Func [] expr, rest)
        [] -> error $ "No tokens found after lambda keyword."
        otherwise -> error $ "Malformed function declaration, expected arguments or '$', found: " ++ (show (head str))

parseAssign :: [Token] -> (Expr, [Token])
parseAssign str =
    case str of
        (_: []) -> error $ "Expected assignment after variable name."
        (VAR name: xs) ->
            case xs of
                (_: []) -> error $ "Expected expression after '='."
                (EQUALS: xs2) ->
                    let (tree, remaining) = parseExpr xs2
                    in
                        (Assignment name tree, remaining)
                otherwise -> error $ "Expected expression after '='."
        otherwise -> error $ "Expected variable name."

parseFuncCall :: [Token] -> (Expr, [Token])
parseFuncCall str =
    let (funcVarUse, left1) = parseExpr str
    in
        case funcVarUse of
            (VarUse funcName) -> parseFuncCallArguments funcVarUse left1
            (Func _ _) ->
                case left1 of
                    (RPAREN: _) -> (funcVarUse, tail left1)
                    otherwise -> error $ "Forgot closing parenthesis on lambda function."
            otherwise -> error $ "Expected function name or lambda, got " ++ (show funcVarUse)

parseFuncCallArguments :: Expr -> [Token] -> (Expr, [Token])
parseFuncCallArguments funcVarUse str =
    case str of
        [] -> error $ "Missing ')'!"
        (RPAREN:xs) -> (FuncCall funcVarUse [], xs)
        otherwise ->
            let (argument, rest) = parseExpr str
            in
                let (FuncCall _ exprs, finalRest) = parseFuncCallArguments funcVarUse rest
                in
                    (FuncCall funcVarUse (argument:exprs), finalRest)

parseVarUse :: [Token] -> (Expr, [Token])
parseVarUse (VAR name:xs) = (VarUse name, xs)

parseIntLiteral :: [Token] -> (Expr, [Token])
parseIntLiteral (INTLIT val:xs) = (IntLiteral val, xs)

parseFloatLiteral :: [Token] -> (Expr, [Token])
parseFloatLiteral (FLOATLIT val:xs) = (FloatLiteral val, xs)

parseBoolLiteral :: [Token] -> (Expr, [Token])
parseBoolLiteral (BOOLLIT val:xs) = (BoolLiteral val, xs)

parseCharLiteral :: [Token] -> (Expr, [Token])
parseCharLiteral (CHARLIT val:xs) = (CharLiteral val, xs)

parseStringLiteral :: [Token] -> (Expr, [Token])
parseStringLiteral (STRINGLIT val:xs) = (ListLiteral $ map CharLiteral val, xs)