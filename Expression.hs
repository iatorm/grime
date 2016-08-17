module Expression where

import Data.Set (Set, toAscList)
import Data.Map (Map)

-- The label of a variable
type Label = Maybe Char

-- Context anchor label; begins from 0
type AnchorLabel = Int

type Coord = (Int, Int) -- x, y
type Size = (Int, Int) -- w, h
type Rect = (Int, Int, Int, Int) -- x, y, w, h
type Range = (Int, Maybe Int)

-- An expression that may or may not match a rectangle of characters
data Expr = Border                   -- Matches the rectangle border symbol
          | AnyRect                  -- Mathces any rectangle
          | AnyChar                  -- Matches any single character (not border)
          | SomeChar Bool
            (Set (Maybe Char))       -- Matches if flag XOR char in set; Nothing matches border
          | Var Label                -- Matches the given variable
          | Expr :> Expr             -- Horizontal concatenation
          | HPlus Expr               -- Horizontal repetition
          | Expr :^ Expr             -- Vertical concatenation
          | VPlus Expr               -- Vertical repetition
          | Expr :| Expr             -- Disjunction
          | Expr :& Expr             -- Conjunction
          | Expr :~ Expr             -- Equivalence (logical)
          | Not Expr                 -- Negation
          | Sized Range Range Expr   -- Size range
          | InContext Expr           -- Context brackets
          | Anchor AnchorLabel       -- Context anchor

instance Show Expr where
  show Border = "b"
  show AnyRect = "$"
  show AnyChar = "."
  show (SomeChar isPos charSet) =
    if isPos
    then "[p:" ++ concatMap (maybe "\\b" $ \c -> if c == '\\' then "\\\\" else [c]) (toAscList charSet) ++ "]"
    else "[n:" ++ concatMap (maybe "\\b" $ \c -> if c == '\\' then "\\\\" else [c]) (toAscList charSet) ++ "]"
  show (Var Nothing) = "_"
  show (Var (Just a)) = [a]
  show (e1 :> e2) = show e1 ++ show e2
  show (HPlus e) = "(" ++ show e ++ ")+"
  show (e1 :^ e2) = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"
  show (VPlus e) = "(" ++ show e ++ ")/+"
  show (e1 :| e2) = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
  show (e1 :& e2) = "(" ++ show e1 ++ "&" ++ show e2 ++ ")"
  show (e1 :~ e2) = "(" ++ show e1 ++ "~" ++ show e2 ++ ")"
  show (Not e) = "(" ++ show e ++ ")!"
  show (Sized (x1,x2) (y1,y2) e) =
    show e ++ "{" ++ show x1 ++ "-" ++ sx2 ++ "," ++ show y1 ++ "-" ++ sy2 ++ "}"
    where sx2 = case x2 of Nothing -> ""; Just x -> show x
          sy2 = case y2 of Nothing -> ""; Just y -> show y
  show (InContext e) = "<" ++ show e ++ ">"
  show (Anchor n) = show n