module Expression where

import Data.Set (Set, toAscList)
import Data.Map (Map)
import Data.Monoid (Monoid(..), (<>))

-- The label of a variable
type Label = Maybe Char

-- Context anchor label; begins from 0
type AnchorLabel = Int

type Coord = (Int, Int) -- x, y
type Size = (Int, Int) -- w, h
type Rect = (Int, Int, Int, Int) -- x, y, w, h
type Range = (Int, Maybe Int)

-- Dihedral group D4 of orientations
-- Rot n is rotation by n*90 degrees
-- RefRot n is Rot n, then reflection by vertical line
data D4 = Rot Int | RefRot Int
  deriving (Eq, Ord)

instance Show D4 where
  show (Rot 0) = ""
  show (Rot n) = "o" ++ show n
  show (RefRot n) = "o" ++ show (n + 4)

axisPreserving :: D4 -> Bool
axisPreserving (Rot 0) = True
axisPreserving (Rot 2) = True
axisPreserving (RefRot 0) = True
axisPreserving (RefRot 2) = True
axisPreserving _ = False

instance Monoid D4 where
  mempty = Rot 0
  mappend (Rot m) (Rot n) = Rot $ (m + n) `mod` 4
  mappend (RefRot m) (Rot n) = RefRot $ (m + n) `mod` 4
  mappend (Rot m) (RefRot n) = RefRot $ (n - m) `mod` 4
  mappend (RefRot m) (RefRot n) = Rot $ (n - m) `mod` 4

-- An expression that may or may not match a rectangle of characters
data Expr = Border                   -- Matches the rectangle border symbol
          | AnyRect                  -- Mathces any rectangle
          | AnyChar                  -- Matches any single character (not border)
          | SomeChar Bool
            (Set (Maybe Char))       -- Matches if flag XOR char in set; Nothing matches border
          | Var D4 Label             -- Matches the given oriented variable
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
          | Fixed Expr               -- Fixed orientation
          deriving (Eq)

instance Show Expr where
  show Border = "b"
  show AnyRect = "$"
  show AnyChar = "."
  show (SomeChar isPos charSet) =
    if isPos
    then "[p:" ++ concatMap (maybe "\\b" $ \c -> if c == '\\' then "\\\\" else [c]) (toAscList charSet) ++ "]"
    else "[n:" ++ concatMap (maybe "\\b" $ \c -> if c == '\\' then "\\\\" else [c]) (toAscList charSet) ++ "]"
  show (Var rot Nothing) = "_" ++ show rot
  show (Var rot (Just a)) = [a] ++ show rot
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
  show (Fixed e) = "(" ++ show e ++ ")oF"

-- Rotate and/or reflect an expression
orient :: Expr -> D4 -> Expr
orient Border _ = Border
orient AnyRect _ = AnyRect
orient AnyChar _ = AnyChar
orient e@(SomeChar _ _) _ = e
orient (Var rot1 label) rot2 = Var (rot2 <> rot1) label
orient e@(e1 :> e2) rot = case rot of
  Rot 0 -> e
  Rot 1 -> orient e2 rot :^ orient e1 rot
  Rot 2 -> orient e2 rot :> orient e1 rot
  Rot 3 -> orient e1 rot :^ orient e2 rot
  RefRot 0 -> orient e2 rot :> orient e1 rot
  RefRot 1 -> orient e2 rot :^ orient e1 rot
  RefRot 2 -> orient e1 rot :> orient e2 rot
  RefRot 3 -> orient e1 rot :^ orient e2 rot
orient (HPlus e) rot = (if axisPreserving rot then HPlus else VPlus) $ orient e rot
orient e@(e1 :^ e2) rot = case rot of
  Rot 0 -> e
  Rot 1 -> orient e1 rot :> orient e2 rot
  Rot 2 -> orient e2 rot :^ orient e1 rot
  Rot 3 -> orient e2 rot :> orient e1 rot
  RefRot 0 -> orient e1 rot :^ orient e2 rot
  RefRot 1 -> orient e2 rot :> orient e1 rot
  RefRot 2 -> orient e2 rot :^ orient e1 rot
  RefRot 3 -> orient e1 rot :> orient e2 rot
orient (VPlus e) rot = (if axisPreserving rot then VPlus else HPlus) $ orient e rot
orient (e1 :| e2) rot = orient e1 rot :| orient e2 rot
orient (e1 :& e2) rot = orient e1 rot :& orient e2 rot
orient (e1 :~ e2) rot = orient e1 rot :~ orient e2 rot
orient (Not e) rot = Not $ orient e rot
orient (Sized (x1,x2) (y1,y2) e) rot =
  if axisPreserving rot
  then Sized (x1,x2) (y1,y2) $ orient e rot
  else Sized (y1,y2) (x1,x2) $ orient e rot
orient (InContext e) rot = InContext $ orient e rot
orient e@(Anchor _) _ = e
orient e@(Fixed _) _ = e
