module Interpreter 
( eval
, evalInEnvironment
, emptyEnvironment
, Environment
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Parser

-- Language Types
data LangType = None | IntType Int | FloatType Float | BoolType Bool | CharType Char
    | FuncType [String] Expr | ListType [LangType]

type Environment = Map String LangType

instance Show LangType where
    show (FloatType val) = show val
    show (IntType val) = show val
    show (BoolType val) = show val
    show (CharType val) = show val
    show (FuncType params body) = "lambda " ++ (show params) ++ " -> " ++ (show body)
    show (ListType elements) = show elements
    show (None) = ""

-- Exported functions
evalInEnvironment :: String -> Environment -> ([LangType], Environment)
evalInEnvironment str env = evalListExprs exprs $ env
    where (Program exprs) = parse str

eval :: String -> ([LangType], Environment)
eval str = evalInEnvironment str emptyEnvironment

emptyEnvironment :: Environment
emptyEnvironment = Map.fromList []

-- Evaluator

-- Takes in an expression and an environment mapping
-- Returns the evaluated expression and an updated environment
evalExpr :: Expr -> Environment -> (LangType, Environment)

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
evalExpr (CharLiteral value) env = (CharType value, env)
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
        Nothing ->
            case Map.lookup name libFuncEnv of
                Nothing -> error $ "Variable '" ++ name ++ "' not declared and not library function."
                Just _ -> evalExpr (Func args (FuncCall (VarUse name) (map VarUse args))) env
                    where args = map (\x -> x:[]) $ take (getNumArgs name) "abcdefghi"
        Just value -> (value, env)


evalExpr Empty env = (None, env)

bindAll :: [String] -> [LangType] -> Environment -> Environment
bindAll [] [] env = env
bindAll [] (x:xs) _ = error $ "Number of arguments doesn't match number of parameters!"
bindAll (x:xs) [] _ = error $ "Number of arguments doesn't match number of parameters!"
bindAll (p:ps) (a:as) env =
    bindAll ps as $ Map.insert p a env

evalListExprs :: [Expr] -> Environment -> ([LangType], Environment)
evalListExprs [] env = ([], env)
evalListExprs (expr:rest) env = 
    let (eval, env1) = evalExpr expr env
    in
        let (restEval, finalEnv) = evalListExprs rest env1
        in
            (eval : restEval, finalEnv)

-- Library Functions

getLibFunc :: String -> (Environment -> [LangType] -> LangType)
getLibFunc name = 
    case Map.lookup name libFuncEnv of
        Nothing -> error $ "Variable '" ++ name ++ "' not declared and is not library function."
        Just value -> value

libFuncEnv :: Map String (Environment -> [LangType] -> LangType)
libFuncEnv = Map.fromList [ ("+", plus), ("-", minus), ("*", times), ("/", libdiv), ("<", lt),
                            ("<=", lte), (">", gt), (">=", gte), ("==", e), ("!=", ne), ("!", libnot),
                            ("&", liband), ("|", libor), ("%", libmod), ("get", liblist_get), ("length", liblist_len),
                            ("map", liblist_map), ("range", liblist_range), ("any", liblist_any),
                            ("all", liblist_all), ("filter", liblist_filter), ("concat", liblist_concat),
                            ("pop", liblist_pop), ("put", liblist_put), ("foldl", liblist_foldl), ("foldr", liblist_foldr),
                            ("slice", liblist_slice)]

getNumArgs :: String -> Int
getNumArgs str =
    case str of
        "!" -> 1
        "length" -> 1
        "any" -> 1
        "all" -> 1
        "+" -> 2
        "-" -> 2
        "*" -> 2
        "/" -> 2
        "%" -> 2
        "<" -> 2
        ">" -> 2
        "&" -> 2
        "|" -> 2
        "<=" -> 2
        ">=" -> 2
        "==" -> 2
        "!=" -> 2
        "get" -> 2
        "pop" -> 2
        "map" -> 2
        "filter" -> 2
        "range" -> 2
        "concat" -> 2
        "foldl" -> 3
        "foldr" -> 3
        "put" -> 3
        "slice" -> 3
        otherwise -> error $ "Unknown library function."

liblist_get :: Environment -> [LangType] -> LangType
liblist_get _ (ListType valList : IntType ind: []) =
    if (length valList) > ind
        then valList !! ind 
        else error $ "List index out of range: " ++ (show ind)
liblist_get _ _ = error $ "get takes exactly two arguments: A ListType and an IntType."

removeIndex [] 0 = error "Cannot remove from empty array"
removeIndex xs n = fst notGlued ++ snd notGlued
    where notGlued = (take n xs, drop (n+1) xs)

insertAtIndex x xs n = take n xs ++ [x] ++ drop n xs

liblist_pop :: Environment -> [LangType] -> LangType
liblist_pop _ (ListType valList : IntType ind: []) =
    if (length valList) > ind
        then ListType $ removeIndex valList ind
        else error $ "List index out of range: " ++ (show ind)
liblist_pop _ _ = error $ "pop takes exactly two arguments: A ListType and an IntType."

liblist_put :: Environment -> [LangType] -> LangType
liblist_put _ (item : ListType valList : IntType ind: []) =
    if (length valList) >= ind
        then ListType $ insertAtIndex item valList ind
        else error $ "List index out of range: " ++ (show ind)
liblist_put _ _ = error $ "put takes exactly three arguments: A LangType, a ListType, and an IntType."

liblist_len :: Environment -> [LangType] -> LangType
liblist_len _ (ListType valList : []) = IntType (length valList)
liblist_len _ _ = error $ "length takes exactly one ListType argument."

liblist_any :: Environment -> [LangType] -> LangType
liblist_any _ (ListType valList : []) = BoolType $ length (filter 
    (\elem -> case elem of
        (BoolType value) -> value
        otherwise -> False)
    valList) > 0
liblist_any _ _ = error $ "any takes exactly one ListType argument."

liblist_all :: Environment -> [LangType] -> LangType
liblist_all _ (ListType valList : []) = BoolType $ length (filter 
    (\elem -> case elem of
        (BoolType value) -> value
        otherwise -> False)
    valList) == (length valList)
liblist_all _ _ = error $ "all takes exactly one ListType argument."

liblist_concat :: Environment -> [LangType] -> LangType
liblist_concat _ [] = error $ "Must have at least one argument."
liblist_concat _ (ListType valList1 : []) = ListType valList1
liblist_concat _ (ListType valList1 : xs) = 
    let (ListType valList2) = liblist_concat Map.empty xs
    in
         ListType $ valList1 ++ valList2
liblist_concat _ _ = error $ "concat takes only ListType arguments."

liblist_range :: Environment -> [LangType] -> LangType
liblist_range _ (IntType start : IntType end : []) = ListType (map IntType [start..end-1])
liblist_range _ (IntType start : IntType end : IntType step : []) = ListType (map IntType [start,start+step..end-1])
liblist_range _ _ = error $ "range takes exactly two or three IntType argument."

liblist_map :: Environment -> [LangType] -> LangType
liblist_map env (FuncType params body : ListType valList : []) =
    if length params /= 1
        then
            error $ "Map function must take exactly one argument."
        else
            ListType (map (\arg -> fst $ evalExpr body $ bindAll params (arg:[]) env) valList)
liblist_map _ _ = error $ "map takes takes exactly two arguments: A FuncType and a ListType."

liblist_filter :: Environment -> [LangType] -> LangType
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

liblist_foldl :: Environment -> [LangType] -> LangType
liblist_foldl env (FuncType params body : startValue : ListType valList : []) =
    if length params /= 2
        then
            error $ "Fold function must take exactly two arguments."
        else
             foldl (\arg1 arg2 -> fst $ evalExpr body $ bindAll params (arg1:arg2:[]) env) startValue valList
liblist_foldl _ _ = error $ "foldl takes takes exactly three arguments: A start value, a FuncType and a ListType."

liblist_foldr :: Environment -> [LangType] -> LangType
liblist_foldr env (FuncType params body : startValue : ListType valList : []) =
    if length params /= 2
        then
            error $ "Fold function must take exactly two arguments."
        else
             foldr (\arg1 arg2 -> fst $ evalExpr body $ bindAll params (arg1:arg2:[]) env) startValue valList
liblist_foldr _ _ = error $ "foldr takes takes exactly three arguments: A start value, a FuncType and a ListType."

liblist_slice :: Environment -> [LangType] -> LangType
liblist_slice env (ListType values : IntType start : IntType end : []) = ListType $ drop start $ take end values
liblist_slice _ _ = error $ "slice takes takes exactly three arguments: A ListType, and two IntTypes."

plus :: Environment -> [LangType] -> LangType
plus = libop (+) (+)

minus :: Environment -> [LangType] -> LangType
minus = libop (-) (-)

times :: Environment -> [LangType] -> LangType
times = libop (*) (*)

libdiv :: Environment -> [LangType] -> LangType
libdiv = libop div (/)

libmod :: Environment -> [LangType] -> LangType
libmod = libop mod (\x y -> error $ "Arguments to mod must be of IntType.")

lt :: Environment -> [LangType] -> LangType
lt = libcmp (<) (<) 

lte :: Environment -> [LangType] -> LangType
lte = libcmp (<=) (<=)

gt :: Environment -> [LangType] -> LangType
gt = libcmp (>) (>)

gte :: Environment -> [LangType] -> LangType
gte = libcmp (>=) (>=)

e :: Environment -> [LangType] -> LangType
e _ (IntType x : IntType x2 : xs) =
    BoolType $ x == x2 &&
    case e Map.empty (IntType x2 : xs) of
        (BoolType val) -> val
e _ (FloatType x : FloatType x2 : xs) =
    BoolType $ x == x2 &&
    case e Map.empty (FloatType x2 : xs) of
        (BoolType val) -> val
e _ (CharType x : CharType x2 : xs) =
    BoolType $ x == x2 &&
    case e Map.empty (CharType x2 : xs) of
        (BoolType val) -> val
e _ (ListType x : ListType x2 : xs) =
    BoolType $ all (\(BoolType val) -> val) (map (\(lt1, lt2) -> e Map.empty [lt1, lt2]) (zip x x2))
    &&
    case e Map.empty (ListType x2 : xs) of
        (BoolType val) -> val
e _ (x:[]) = BoolType True
e _ _ = BoolType False


ne :: Environment -> [LangType] -> LangType
ne env inputs = case e env inputs of
    (BoolType value) -> BoolType $ not value

libnot :: Environment -> [LangType] -> LangType
libnot _ (BoolType val : []) = BoolType $ not val
libnot _ (_:[]) = error $ "Unsupported operand type."
libnot _ _ = error $ "! takes exactly one argument."

liband :: Environment -> [LangType] -> LangType
liband _ [] = BoolType True
liband _ (BoolType val : xs) =
    let (BoolType val2) = liband Map.empty xs
    in
        BoolType (val && val2)
liband _ (_:xs) = error $ "Unsupported operand type."

libor :: Environment -> [LangType] -> LangType
libor _ [] = BoolType False
libor _ (BoolType val : xs) =
    let (BoolType val2) = libor Map.empty xs
    in
        BoolType (val || val2)
libor _ (_:xs) = error $ "Unsupported operand type."

libop :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Environment -> [LangType] -> LangType
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

libcmp :: (Int -> Int -> Bool) -> (Float -> Float -> Bool) -> Environment -> [LangType] -> LangType
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
libcmp _ _ _ (_:xs) = error $ "Unsupported operand type."