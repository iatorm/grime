module Expression where

import Data.Set (Set, toAscList)
import Data.Map (Map)

-- The label of a variable
type Label = Maybe Char

type Coord = (Int, Int) -- x, y
type Size = (Int, Int) -- w, h
type Rect = (Int, Int, Int, Int) -- x, y, w, h
type Range = (Int, Maybe Int)

-- An expression that may or may not match a rectangle of characters
data Expr = Border                   -- Matches the rectangle border symbol
          | AnyRect                  -- Mathces any rectangle
          | AnyChar                  -- Matches any single character (not border)
          | SomeChar Bool (Set Char) -- Matches if flag XOR char in set
          | Var Label                -- Matches the given variable
          | Expr :> Expr             -- Horizontal concatenation
          | HPlus Expr               -- Horizontal repetition
          | Expr :^ Expr             -- Vertical concatenation
          | VPlus Expr               -- Vertical repetition
          | Expr :| Expr             -- Disjunction
          | Expr :& Expr             -- Conjunction
          | Not Expr                 -- Negation
          | Sized Range Range Expr   -- Size range

instance Show Expr where
  show Border = "b"
  show AnyRect = "$"
  show AnyChar = "."
  show (SomeChar isPos charSet) =
    if isPos
    then "[p:" ++ toAscList charSet ++ "]"
    else "[n:" ++ toAscList charSet ++ "]"
  show (Var Nothing) = ""
  show (Var (Just a)) = [a]
  show (e1 :> e2) = show e1 ++ show e2
  show (HPlus e) = "(" ++ show e ++ ")+"
  show (e1 :^ e2) = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"
  show (VPlus e) = "(" ++ show e ++ ")/+"
  show (e1 :| e2) = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
  show (e1 :& e2) = "(" ++ show e1 ++ "&" ++ show e2 ++ ")"
  show (Not e) = "(" ++ show e ++ ")!"
  show (Sized (x1,x2) (y1,y2) e) =
    show e ++ "{" ++ show x1 ++ "-" ++ sx2 ++ "," ++ show y1 ++ "-" ++ sy2 ++ "}"
    where sx2 = case x2 of Nothing -> ""; Just x -> show x
          sy2 = case y2 of Nothing -> ""; Just y -> show y

-- An unchanging context for matching in a matrix
data Context = Context {size :: Size,
                        matrix :: Map Coord Char,
                        clauses :: Map Label Expr}
            deriving (Show)