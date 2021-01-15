module Eval where
-- This file contains definitions for functions and operators

import Val
import Data.Char

-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> [Val] -> [Val]
-- Multiplication
-- if arguments are integers, keep result as integer
eval "*" (Integer x: Integer y:tl) = Integer (x*y) : tl
-- if any argument is float, make result a float
eval "*" (x:y:tl) = (Real $ toFloat x * toFloat y) : tl 
-- any remaining cases are stacks too short
eval "*" _ = error("Stack underflow")

-- If arguments are ints have ints.
eval "+" (Integer x: Integer y:tl) = Integer (x+y) : tl
-- If arguments are floats turn.
eval "+" (x:y:tl) = (Real $ toFloat x + toFloat y) : tl 
-- any remaining cases are stacks too short
eval "+" _ = error("Stack underflow")
-- Duplicate the element at the top of the stack

-- If arguments are ints have ints.
eval "-" (Integer x: Integer y:tl) = Integer (x-y) : tl
-- If arguments are floats turn.
eval "-" (x:y:tl) = (Real $ toFloat x - toFloat y) : tl 
-- any remaining cases are stacks too short
eval "-" _ = error("Stack underflow")
-- Duplicate the element at the top of the stack

eval "^" (Integer x: Integer y:tl) = Integer (x^y) : tl
-- If arguments are floats turn.
eval "^" (x:y:tl) = (Real $ toFloat x ** toFloat y) : tl 
-- any remaining cases are stacks too short
eval "^" _ = error("Stack underflow")

eval "/" (Integer x: Integer y:tl) = Integer (x `div` y) : tl
-- If arguments are floats turn.
eval "/" (x:y:tl) = (Real $ toFloat x / toFloat y) : tl 
-- any remaining cases are stacks too short
eval "/" _ = error("Stack underflow")

eval "CONCAT2"(Id x: Id y:tl) = Id (x ++ y) : tl

eval "CONCAT3"(Id x: Id y: Id z:tl) = Id (x ++ y ++ z) : tl

eval "STR" (Integer x:tl) = (Id (show x)) :tl

eval "STR" (Real x:tl) = (Id (show x)) :tl

eval "STR" (Id x:tl) = (Id x):tl

eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack underflow")

-- this must be the last rule
-- it assumes that no match is made and preserves the string as argument
eval s l = Id s : l

--check out show library

-- variant of eval with output
-- state is a stack and string pair
evalOut :: String -> ([Val], String) -> ([Val], String) 


-- print element at the top of the stack
--evalOut "." (Id x:tl, out) = (tl, out ++ x)
evalOut "." (Integer x:tl, out) = (tl, out ++ (show x))
evalOut "." (Real x:tl, out) = (tl, out ++ (show x))
evalOut "." (Id x:tl, out) = (tl, out ++ show x)

evalOut "." ([], _) = error "Stack underflow"
evalOut "CR" (l, out) = (l, out ++ "\n")

evalOut "EMIT" (Integer x:tl, out) = (tl, out ++ [chr x])
evalOut "EMIT" (Id x:tl, out) = error "You passed string"
-- this has to be the last case
-- if no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)
