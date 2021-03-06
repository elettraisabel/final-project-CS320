module Parse where

import AbstractSyntax

type Token = String

tokenize :: String -> [Token]
tokenize s =
  let splits = [0] ++ concat [[i,i+1] | i <- [0..length s-1], s!!i `elem` ";(), "] ++ [length s]
      all = [ [s!!i | i <- [splits!!k..(splits!!(k+1)-1)]] | k <- [0..length splits-2] ]
  in [token | token <- all, token /= " " && token /= ""]

class Parseable a where
  parse :: [Token] -> Maybe (a, [Token])

-- Fill in the missing cases in the definition below for Problem #1, part (a).
instance Parseable Exp where
  parse (t:ts) =
    if t == "and" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e1, ts) = r
          in if length ts == 0 || ts!!0 /= "," then Nothing else
              let r = parse (tail ts)
              in if r == Nothing then Nothing else
                  let Just (e2, ts) = r
                  in if ts!!0 /= ")" then Nothing else
                      Just (And e1 e2, tail ts)
    else if t == "or" && length ts > 0 && ts!!0 == "(" then
      let r = parse(tail ts)
      in if r == Nothing then Nothing else
          let Just(e1, ts) = r
          in if length ts == 0 || ts!!0 /= "," then Nothing else
              let r = parse (tail ts)
              in if r == Nothing then Nothing else
                  let Just(e2, ts) = r
                  in if ts!!0 /= ")" then Nothing else
                      Just (Or e1 e2, tail ts)
    else if t == "not" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
          in if length ts == 0 || ts!!0 /= ")" then Nothing else
              Just (Not e, tail ts)
    else if t == "true" then
      Just (Value True, ts)
    else if t == "false" then
      Just(Value False, ts)
    else if subset t "abcdefghijklmnopqrstuvwxyz" then
      Just (Variable t, ts)
    else
      Nothing
  parse _ = Nothing

-- Fill in the missing cases in the definition below for Problem #1, part (a).
instance Parseable Stmt where
  parse (t:ts) =
    if t == "end" && ts == [";"] then
      Just (End, [])
    else if t == "assign" && length ts > 1 && ts!!0 `subset` "abcdefghijklmnopqrstuvwxyz" && ts!!1 == ":=" then
      let x = ts!!0
          r = parse (drop 2 ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
              r' = parse (tail ts)
          in if r' == Nothing || ts!!0 /= ";" then Nothing else
              let Just (s, ts) = r'
              in Just (Assign x e s, ts)
    else if t == "print" && length ts > 1 then
      let r = parse(ts)
      in if r == Nothing then Nothing else
          let Just(e, ts) = r
              r' = parse (tail ts)
          in if r' == Nothing || ts!!0 /= ";" then Nothing else
              let Just (s, ts) = r'
              in Just (Print e s, ts)

    else
      Nothing
  parse _ = Nothing

tokenizeParse :: (Eq a, Parseable a) => String -> Maybe a
tokenizeParse s =
  let r = parse (tokenize s)
  in if r == Nothing then Nothing else
    let Just (x, ts) = r
    in if length ts > 0 then Nothing else Just x

-- Useful helper functions.
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = and [x `elem` ys | x <- xs]

-- Example of a concrete syntax string being parsed.
example = fst $ (\(Just x)->x) $ parse (tokenize "assign x := not(and(true, false)); print x; assign a := not(and(x, x)); print a; end;") :: Stmt
example2 = fst $ (\(Just x)->x) $ parse (tokenize "assign x := y; print x; print z; end;") :: Stmt
example3 = fst $ (\(Just x)->x) $ parse (tokenize "assign y := true; assign x := y; print x; print z; end;") :: Stmt

-- Problem 1, part (a).
kindOfParser = "predictive" -- "backtracking" or "predictive"

-- eof