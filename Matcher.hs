module Matcher where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, empty, lookup, insert)
import Data.Set (Set, member, toAscList)
import Control.Monad (filterM)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State.Lazy (State, gets, modify, evalState)

-- The label of a variable
type Label = Maybe Char

type Coord = (Int, Int) -- x, y
type Size = (Int, Int) -- w, h
type Rect = (Int, Int, Int, Int) -- x, y, w, h
type Range = (Int, Maybe Int)

-- An expression that may or may not match a rectangle of characters
data Expr = Border                 -- Matches the rectangle border symbol
          | AnyRect                -- Mathces any rectangle
          | AnyChar                -- Matches any single character (not border)
          | SomeChar (Set Char)    -- Matches any of the given characters
          | Var Label              -- Matches the given variable
          | Expr :> Expr           -- Horizontal concatenation
          | HPlus Expr             -- Horizontal repetition
          | Expr :^ Expr           -- Vertical concatenation
          | VPlus Expr             -- Vertical repetition
          | Expr :| Expr           -- Disjunction
          | Expr :& Expr           -- Conjunction
          | Not Expr               -- Negation
          | Sized Range Range Expr -- Size range

instance Show Expr where
  show Border = "b"
  show AnyRect = "$"
  show AnyChar = "."
  show (SomeChar set) = "[" ++ toAscList set ++ "]"
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

-- A memoization of matches
type Classification = Map (Rect, Label) Bool
  
-- An unchanging context for matching in a matrix
data Context = Context {size :: Size,
                        matrix :: Map Coord Char,
                        clauses :: Map Label Expr}
            deriving (Show)

-- A monad for performing matching in a matrix
type Matcher a = ReaderT Context (State Classification) a

-- Short-circuiting monadic Boolean functions
(&?) :: (Monad m) => m Bool -> m Bool -> m Bool
f &? g = do
  a <- f
  if a then g else return False

(|?) :: (Monad m) => m Bool -> m Bool -> m Bool
f |? g = do
  a <- f
  if not a then g else return True

anyM :: (Monad m) => [a] -> (a -> m Bool) -> m Bool
anyM xs f = foldr (|?) (return False) $ map f xs

allM :: (Monad m) => [a] -> (a -> m Bool) -> m Bool
allM xs f = foldr (&?) (return True) $ map f xs

-- Does the pattern match? Update all sub-rectangles as needed
matches :: Expr -> Rect -> Matcher Bool
matches Border (x, y, 1, 1) = do
  ch <- asks $ lookup (x, y) . matrix
  return $ case ch of
    Nothing -> True
    Just _ -> False
matches Border _ = return False
matches AnyRect _ = return True
matches AnyChar (x, y, 1, 1) = do
  ch <- asks $ lookup (x, y) . matrix
  return $ case ch of
    Nothing -> False
    Just _ -> True
matches AnyChar _ = return False
matches (SomeChar cs) (x, y, 1, 1) = do
  ch <- asks $ lookup (x, y) . matrix
  return $ case ch of
      Just c -> c `member` cs
      Nothing -> False
matches (SomeChar _) _ = return False
matches (Var label) rect = do
  memoed <- gets $ lookup (rect, label)
  case memoed of
    Just b -> return b
    Nothing -> do
      modify $ insert (rect, label) False
      Just expr <- asks $ lookup label . clauses
      match <- matches expr rect
      modify $ insert (rect, label) match
      return  match
matches (lExp :> rExp) (x, y, w, h) = anyM [0..w] $ \i ->
  matches lExp (x, y, i, h) &? matches rExp (x+i, y, w-i, h)
matches (HPlus expr) r@(x, y, w, h) = whole |? parts
  where whole = matches expr r
        parts = anyM [1..w-1] $ \i ->
          matches expr (x, y, i, h) &? matches (HPlus expr) (x+i, y, w-i, h)
matches (dExp :^ uExp) (x, y, w, h) = anyM [0..h] $ \j ->
  matches dExp (x, y, w, j) &? matches uExp (x, y+j, w, h-j)
matches (VPlus expr) r@(x, y, w, h) = whole |? parts
  where whole = matches expr r
        parts = anyM [1..h-1] $ \j ->
          matches expr (x, y, w, j) &? matches (VPlus expr) (x, y+j, w, h-j)
matches (exp1 :| exp2) rect = matches exp1 rect |? matches exp2 rect
matches (exp1 :& exp2) rect = matches exp1 rect &? matches exp2 rect
matches (Not expr) rect = fmap not $ matches expr rect
matches (Sized (x1, x2) (y1, y2) exp) r@(x, y, w, h) = do
  let xOk = x1 <= w && case x2 of Nothing -> True; Just x3 -> w <= x3
      yOk = y1 <= h && case y2 of Nothing -> True; Just y3 -> h <= y3
  return (xOk && yOk) &? case exp of
    Border -> allMatch
    AnyChar -> allMatch
    SomeChar _ -> allMatch
    _ -> matches exp r
  where allMatch = allM [(x+i, y+j, 1, 1) | i <- [0..w-1], j <- [0..h-1]] $
                   matches exp


-- Collect matches of Nothing for the given rectangles.
matchAllEmpty :: Context -> [Rect] -> [Rect]
matchAllEmpty con rects = flip evalState empty . flip runReaderT con $ do
  Just expr <- asks $ lookup Nothing . clauses
  filterM (matches expr) rects
