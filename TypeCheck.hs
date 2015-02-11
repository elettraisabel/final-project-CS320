module TypeCheck where

import AbstractSyntax
import Parse
import Data.Maybe
class Typeable a where
  chk :: [(String, Type)] -> a -> Maybe Type
  
instance Typeable Exp where 
  chk tau (And e1 e2)  = if (chk tau e1 /= Just Bool) && (chk tau e2 /= Just Bool )
  							then Nothing else Just Bool          -- Implement for Problem #1, part (c).
  chk tau (Or  e1 e2)  = if (chk tau e1 /= Just Bool) && (chk tau e2 /= Just Bool)
  							then Nothing else Just Bool
  chk tau (Not   e  )  = if (chk tau e /= Just Bool) then Nothing else Just Bool
  chk tau (Variable x) = Just (lookup' x tau)
  chk tau (Value t) = Just Bool

instance Typeable Stmt where 
  chk tau (Print s e)    = if (chk tau s /= Just(Void)) && (chk tau e /= Just (Bool) )
  								then Nothing else Just Void -- Implement for Problem #1, part (c).
  chk tau (Assign x s e) = let t = fromJust (chk tau e)
  							in if chk (tau++[(x, t)]) s /= Just Void then Nothing else Just Void
  chk tau (End) = Just Void


-- eof