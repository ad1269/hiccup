module Parser where
import Text.Read
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map

-- Lexer Tokens

data Token = INTLIT Int | FLOATLIT Float | BOOLLIT Bool | KEYWORD_ASSIGN | KEYWORD_LAMBDA | KEYWORD_IF
    | KEYWORD_THEN | KEYWORD_ELSE | VAR String | LPAREN | RPAREN | LBRACKET | RBRACKET | COMMA
    | EQUALS | SEMICOLON | DOLLAR
    deriving (Show, Eq)

-- Language Grammar
data Program = Program [Expr] deriving (Show)

-- An expression evaluates to a value
data Expr = Empty 
    | IntLiteral Int
    | FloatLiteral Float
    | BoolLiteral Bool
    | ListLiteral [Expr] -- Evaluates to a ListType object
    | VarUse String -- Evaluates to variable's value
    | FuncCall Expr [Expr] -- First argument must evaluate to VarUse
    | Assignment String Expr -- Evaluates to the expression on its right side
    | Func [String] Expr -- Evaluates to a FuncType object
    | Conditional Expr Expr Expr -- First argument must evaluate to a BoolType
    deriving (Show)

-- Language Types
-- TODO: Add support for strings and lists.
data LangType = None | IntType Int | FloatType Float | BoolType Bool
    | FuncType [String] Expr | ListType [LangType]

instance Show LangType where
    show (FloatType val) = show val
    show (IntType val) = show val
    show (BoolType val) = show val
    show (FuncType params body) = "lambda " ++ (show params) ++ " -> " ++ (show body)
    show (ListType elements) = show elements
    show (None) = ""

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

parse :: [Token] -> Program
parse str =
    if verify result
    then
        let (trees, _) = unzip result
        in
        Program trees
    else error $ "Check semicolons."
    where result = map parseExpr (splitBy str SEMICOLON)

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

-- Evaluator

-- Takes in an expression and an environment mapping
-- Returns the evaluated expression and an updated environment
evalExpr :: Expr -> Map String LangType -> (LangType, Map String LangType)

evalExpr (FuncCall funcVarUse arguments) env =
    let (VarUse funcName) = funcVarUse
        (evaluatedArgs, newEnv) = evalListExprs arguments env
    in
        if elem funcName (Map.keys env)
        then
            let (evaluatedFunc, _) = evalExpr funcVarUse env
            in
                case evaluatedFunc of
                    (FuncType paramNames body) ->
                            -- Bind evaluatedArgs to paramNames
                            -- Evaluate body under this environment and return the result
                            -- Discard the environment we evaluated the function with
                            let (returnValue, _) = evalExpr body (bindAll paramNames evaluatedArgs newEnv)
                            in
                                (returnValue, newEnv)
                    otherwise -> error $ "Trying to call a non-callable object, expected FuncType, got: " ++ (show evaluatedFunc)
        else
            -- Check if this is a library function
            let func = getLibFunc funcName
            in
                (func newEnv evaluatedArgs, newEnv)

evalExpr (IntLiteral value) env = (IntType value, env)
evalExpr (FloatLiteral value) env = (FloatType value, env)
evalExpr (BoolLiteral value) env = (BoolType value, env)
evalExpr (Func params body) env = (FuncType params body, env)

evalExpr (ListLiteral elements) env = 
    let (evalElems, newEnv) = evalListExprs elements env
    in
        (ListType evalElems, newEnv)

evalExpr (Conditional condExpr primaryExpr altExpr) env =
    let (condValue, env1) = evalExpr condExpr env
    in
        case condValue of
            (BoolType True) -> evalExpr primaryExpr env1
            (BoolType False) -> evalExpr altExpr env1
            otherwise -> error $ "Expression after IF must be BoolType."

evalExpr (Assignment name expr) env =
    let (value, env1) = evalExpr expr env
    in
        (value, Map.insert name value env1)

evalExpr (VarUse name) env =
    case Map.lookup name env of
        Nothing -> error $ "Variable '" ++ name ++ "' not declared."
        Just value -> (value, env)

evalExpr Empty env = (None, env)

bindAll :: [String] -> [LangType] -> Map String LangType -> Map String LangType
bindAll [] [] env = env
bindAll [] (x:xs) _ = error $ "Number of arguments doesn't match number of parameters!"
bindAll (x:xs) [] _ = error $ "Number of arguments doesn't match number of parameters!"
bindAll (p:ps) (a:as) env =
    bindAll ps as $ Map.insert p a env

evalListExprs :: [Expr] -> Map String LangType -> ([LangType], Map String LangType)
evalListExprs [] env = ([], env)
evalListExprs (expr:rest) env = 
    let (eval, env1) = evalExpr expr env
    in
        let (restEval, finalEnv) = evalListExprs rest env1
        in
            (eval : restEval, finalEnv)

evalProg :: Program -> Map String LangType -> ([LangType], Map String LangType)
evalProg (Program exprs) env = evalListExprs exprs $ env

eval :: String -> ([LangType], Map String LangType)
eval str = evalProg (parse $ tokenize str) (Map.fromList [])

-- Library Functions

getLibFunc :: String -> (Map String LangType -> [LangType] -> LangType)
getLibFunc name = 
    case Map.lookup name libFuncEnv of
        Nothing -> error $ "Variable '" ++ name ++ "' not declared and is not library function."
        Just value -> value

libFuncEnv :: Map String (Map String LangType -> [LangType] -> LangType)
libFuncEnv = Map.fromList [ ("+", plus), ("-", minus), ("*", times), ("/", libdiv), ("<", lt),
                            ("<=", lte), (">", gt), (">=", gte), ("==", e), ("!=", ne), ("!", libnot),
                            ("&", liband), ("|", libor), ("%", libmod), ("get", liblist_get), ("length", liblist_len),
                            ("map", liblist_map), ("range", liblist_range), ("any", liblist_any),
                            ("all", liblist_all), ("filter", liblist_filter), ("concat", liblist_concat),
                            ("pop", liblist_pop), ("put", liblist_put)]

liblist_get :: Map String LangType -> [LangType] -> LangType
liblist_get _ (ListType valList : IntType ind: []) =
    if (length valList) > ind
        then valList !! ind 
        else error $ "List index out of range."
liblist_get _ _ = error $ "get takes exactly two arguments: A ListType and an IntType."

removeIndex [] 0 = error "Cannot remove from empty array"
removeIndex xs n = fst notGlued ++ snd notGlued
    where notGlued = (take n xs, drop (n+1) xs)

insertAtIndex x xs n = take n xs ++ [x] ++ drop n xs

liblist_pop :: Map String LangType -> [LangType] -> LangType
liblist_pop _ (ListType valList : IntType ind: []) =
    if (length valList) > ind
        then ListType $ removeIndex valList ind
        else error $ "List index out of range."
liblist_pop _ _ = error $ "pop takes exactly two arguments: A ListType and an IntType."

liblist_put :: Map String LangType -> [LangType] -> LangType
liblist_put _ (item : ListType valList : IntType ind: []) =
    if (length valList) > ind
        then ListType $ insertAtIndex item valList ind
        else error $ "List index out of range."
liblist_put _ _ = error $ "put takes exactly three arguments: A LangType, a ListType, and an IntType."

liblist_len :: Map String LangType -> [LangType] -> LangType
liblist_len _ (ListType valList : []) = IntType (length valList)
liblist_len _ _ = error $ "length takes exactly one ListType argument."

liblist_any :: Map String LangType -> [LangType] -> LangType
liblist_any _ (ListType valList : []) = BoolType $ length (filter 
    (\elem -> case elem of
        (BoolType value) -> value
        otherwise -> False)
    valList) > 0
liblist_any _ _ = error $ "any takes exactly one ListType argument."

liblist_all :: Map String LangType -> [LangType] -> LangType
liblist_all _ (ListType valList : []) = BoolType $ length (filter 
    (\elem -> case elem of
        (BoolType value) -> value
        otherwise -> False)
    valList) == (length valList)
liblist_all _ _ = error $ "all takes exactly one ListType argument."

liblist_concat :: Map String LangType -> [LangType] -> LangType
liblist_concat _ [] = error $ "Must have at least one argument."
liblist_concat _ (ListType valList1 : []) = ListType valList1
liblist_concat _ (ListType valList1 : xs) = 
    let (ListType valList2) = liblist_concat Map.empty xs
    in
         ListType $ valList1 ++ valList2
liblist_concat _ _ = error $ "concat takes only ListType arguments."

liblist_range :: Map String LangType -> [LangType] -> LangType
liblist_range _ (IntType start : IntType end : []) = ListType (map IntType [start..end-1])
liblist_range _ (IntType start : IntType end : IntType step : []) = ListType (map IntType [start,start+step..end-1])
liblist_range _ _ = error $ "range takes exactly two or three IntType argument."

liblist_map :: Map String LangType -> [LangType] -> LangType
liblist_map env (FuncType params body : ListType valList : []) =
    if length params /= 1
        then
            error $ "Map function must take exactly one argument."
        else
            ListType (map (\arg -> fst $ evalExpr body $ bindAll params (arg:[]) env) valList)
liblist_map _ _ = error $ "map takes takes exactly two arguments: A FuncType and a ListType."

liblist_filter :: Map String LangType -> [LangType] -> LangType
liblist_filter env (FuncType params body : ListType valList : []) =
    if length params /= 1
        then
            error $ "Filter function must take exactly one argument."
        else
            ListType (filter (\arg -> 
                            case fst $ evalExpr body $ bindAll params (arg:[]) env of
                                (BoolType value) -> value
                                otherwise -> False)
                        valList)
liblist_filter _ _ = error $ "filter takes takes exactly two arguments: A FuncType and a ListType."

plus :: Map String LangType -> [LangType] -> LangType
plus = libop (+) (+)

minus :: Map String LangType -> [LangType] -> LangType
minus = libop (-) (-)

times :: Map String LangType -> [LangType] -> LangType
times = libop (*) (*)

libdiv :: Map String LangType -> [LangType] -> LangType
libdiv = libop div (/)

libmod :: Map String LangType -> [LangType] -> LangType
libmod = libop mod (\x y -> error $ "Arguments to mod must be of IntType.")

lt :: Map String LangType -> [LangType] -> LangType
lt = libcmp (<) (<) 

lte :: Map String LangType -> [LangType] -> LangType
lte = libcmp (<=) (<=)

gt :: Map String LangType -> [LangType] -> LangType
gt = libcmp (>) (>)

gte :: Map String LangType -> [LangType] -> LangType
gte = libcmp (>=) (>=)

e :: Map String LangType -> [LangType] -> LangType
e = libcmp (==) (==)

ne :: Map String LangType -> [LangType] -> LangType
ne = libcmp (/=) (/=)

libnot :: Map String LangType -> [LangType] -> LangType
libnot _ (BoolType val : []) = BoolType $ not val
libnot _ (_:[]) = error $ "Unsupported operand type."
libnot _ _ = error $ "! takes exactly one argument."

liband :: Map String LangType -> [LangType] -> LangType
liband _ [] = BoolType True
liband _ (BoolType val : xs) =
    let (BoolType val2) = liband Map.empty xs
    in
        BoolType (val && val2)
liband _ (_:xs) = error $ "Unsupported operand type."

libor :: Map String LangType -> [LangType] -> LangType
libor _ [] = BoolType False
libor _ (BoolType val : xs) =
    let (BoolType val2) = libor Map.empty xs
    in
        BoolType (val || val2)
libor _ (_:xs) = error $ "Unsupported operand type."

libop :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Map String LangType -> [LangType] -> LangType
libop _ _ _ [] = error $ "Must have at least one argument."
libop _ _ _ (ltype:[]) = ltype
libop intopfunc fltopfunc _ (IntType val:xs) =
    case libop intopfunc fltopfunc Map.empty xs of
        (IntType val2) -> IntType (val `intopfunc` val2)
        (FloatType val2) -> FloatType ((fromIntegral val :: Float) `fltopfunc` val2)
libop intopfunc fltopfunc _ (FloatType val:xs) =
    case libop intopfunc fltopfunc Map.empty xs of
        (IntType val2) -> FloatType (val `fltopfunc` (fromIntegral val2 :: Float))
        (FloatType val2) -> FloatType (val `fltopfunc` val2)
libop _ _ _ (_:xs) = error $ "Unsupported operand type."

libcmp :: (Int -> Int -> Bool) -> (Float -> Float -> Bool) -> Map String LangType -> [LangType] -> LangType
libcmp _ _ _ [] = BoolType True
libcmp intcmpfunc fltcmpfunc _ (IntType val1:xs) =
    case libcmp intcmpfunc fltcmpfunc Map.empty xs of
        (BoolType False) -> (BoolType False)
        otherwise ->
            case xs of
                [] -> (BoolType True)
                (IntType val2:xs2) -> BoolType (val1 `intcmpfunc` val2)
                (FloatType val2:xs2) -> BoolType ((fromIntegral val1 :: Float) `fltcmpfunc` val2)
libcmp intcmpfunc fltcmpfunc _ (FloatType val1:xs) =
    case libcmp intcmpfunc fltcmpfunc Map.empty xs of
        (BoolType False) -> (BoolType False)
        otherwise ->
            case xs of
                [] -> (BoolType True)
                (IntType val2:xs2) -> BoolType (val1 `fltcmpfunc` (fromIntegral val2 :: Float))
                (FloatType val2:xs2) -> BoolType (val1 `fltcmpfunc` val2)
libcmp _ _ _ (_:xs) =  error $ "Unsupported operand type."
