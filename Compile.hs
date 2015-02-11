module Compile where

import AbstractSyntax
import Allocation
import Machine
import TypeCheck

class Compilable a where
  comp :: [(Var, Register)] -> a -> Instruction

instance Compilable Stmt where
  comp xrs (End) = STOP (Register 0) -- Complete missing cases for Problem #4, part (b).
  comp xrs (Print e s) = COPY (Register 0) (register (comp xrs e)) (comp xrs s)
  comp xrs (Assign x e s ) = COPY (lookup' x xrs) (register (comp xrs e)) (comp xrs s)
instance Compilable Exp where
  comp xrs (Variable x) = STOP (lookup' x xrs)
  comp xrs (Value v) = let nextReg = maximum ([r | (var, r) <- xrs]) + 1 in if v == False then INIT (nextReg) (STOP nextReg) -- Complete missing cases for Problem #4, part (b).
                          else INIT (nextReg) (FLIP nextReg (STOP nextReg))
  comp xrs (And e1 e2 ) = let nextReg = maximum [r | (x, r) <- xrs] + 1 in NAND (register (comp ([("nextRegFirst", nextReg)]++xrs) e1)) (register (comp ([("nextRegSecond", nextReg + 1)]++xrs) e2)) (nextReg + 2) (FLIP (nextReg +2) (STOP (nextReg+2)))
  comp xrs (Or e1 e2) = let nextReg = maximum [r | (x, r) <- xrs] + 1 in  (FLIP ( register (comp ([("nextRegFirst", nextReg)]++xrs) e1)) (FLIP (register (comp ([("nextRegSecond", nextReg + 1)]++xrs) e2)) (NAND (nextReg) (nextReg+1) (nextReg +2) (STOP (nextReg+2) ))))
  comp xrs (Not e) = let nextReg = maximum [r | (x,r) <- xrs] + 1 in FLIP ( register (comp ([("nextReg", nextReg)]++xrs) e)) (STOP (nextReg+1))
--compileMin :: Stmt -> Maybe Instruction
--compileMin _ = STOP (Register -1) -- Complete for Problem #4, part (c).

--compileMax :: Integer -> Stmt -> Maybe Instruction
---compileMax _ _ = Nothing -- Complete for Problem #4, part (d).

-- eof