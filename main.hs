module Main where
import System.Environment
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

import Parser

main :: IO ()
main = do
    repl $ Map.fromList []

repl :: Map String LangType -> IO ()
repl env = do
    putStr ">>> "
    hFlush stdout
    expression <- getLine
    let (value, newEnv) = evalProg (parse $ tokenize expression) env
    putStrLn (show value)
    repl newEnv



-- Programs with conditionals, booleans, and function calls

-- assign factorial = lambda n $
--     if (<= n 0)
--         then
--             1
--         else
--             (* 
--                 n 
--                 (factorial (- n 1))
--             )

-- assign fib = lambda n $
--     if (<= n 2)
--         then
--             1 
--         else
--             (+ 
--                 (fib (- n 1)) 
--                 (fib (- n 2))
--             )

-- Basic lists and list functions
-- Functions: [get, length]
-- TODO: [Implement put, pop, any, all, map, range, filter, reduce, concat]

-- assign qsort = lambda lst $
--     if (<= (length lst) 1)
--         then
--             lst
--         else
--             (concat
--                 (qsort (filter lst (lambda x $ (< x (get lst 0)))))
--                 (qsort (filter lst (lambda x $ (>= x (get lst 0)))))
--             )

-- Partial application of functions & anonymous lambda functions

-- TODO: [Convert to use anonymous functions]
-- assign isNotDiv = lambda a b $ (! (% a b)) 
-- assign isPrime = lambda n $ if (n <= 3) then true else (any (map (isNotDiv n) (range 2 n)))

-- String algorithms
-- TODO: Unwrap string literals to list literals