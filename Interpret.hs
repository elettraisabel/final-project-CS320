module Interpret where

import AbstractSyntax
import Parse
import TypeCheck

eval :: [(String, Bool)] -> Exp -> Bool
eval env (Or e1 e2) = eval env e1 || eval env e2 
eval env (And e1 e2) = eval env e1 && eval env e2 -- Implement for Problem #1, part (b).
eval env (Not e) = not (eval env e)
eval env (Variable x) = lookup' x env 
eval env (Value t) = t -- will this work?


exec :: [(String, Bool)] -> Stmt -> ([(String, Bool)], Output)
exec env (Print    e s) =
  let (env', o) = exec env s
  in (env', [eval env e] ++ o)
exec env (Assign x e s) = 
	let (env', o) = exec (env++(x, eval env e)) s
	in (env', o) --do we need to unpack it though?
exec env (End) = (env, []) -- Implement the Assign and End cases for Problem #1, part (b).

interpret :: Stmt -> Maybe Output
interpret s = let check = chk [] s 
			  in if check /= Just Void then Nothing else
			  	let (env,o) = exec [] s
			  	in o

lookup' :: String -> [(String, Bool)] -> Bool
lookup' x ((x',i) : rest) = if x == x' then i else lookup' x rest
-- eof