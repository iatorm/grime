module Matcher where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, empty, lookup, insert)
import qualified Data.Map.Strict as Map (filter)
import Data.Set (Set, member, toAscList)
import Data.Monoid (Any(Any), getAny)
import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.Lazy (WriterT, tell, listen, listens, runWriterT)
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

-- A fuzzy match
data Match = Match
           | NoMatch
           | Unknown
           deriving (Eq,Show)

invert :: Match -> Match
invert Match = NoMatch
invert NoMatch = Match
invert Unknown = Unknown

-- Short-circuiting monadic utility functions for fuzzy matches
(&?) :: (Monad m) => m Match -> m Match -> m Match
f &? g = do
  a <- f
  case a of
   NoMatch -> return NoMatch
   _ -> do
     b <- g
     return $ case (a,b) of
      (_, NoMatch) -> NoMatch
      (Match, Match) -> Match
      _ -> Unknown

(|?) :: (Monad m) => m Match -> m Match -> m Match
f |? g = do
  a <- f
  case a of
   Match -> return Match
   _ -> do
     b <- g
     return $ case (a,b) of
      (_, Match) -> Match
      (NoMatch, NoMatch) -> NoMatch
      _ -> Unknown

anyM :: (Monad m) => [a] -> (a -> m Match) -> m Match
anyM xs f = foldr (|?) (return NoMatch) $ f <$> xs

allM :: (Monad m) => [a] -> (a -> m Match) -> m Match
allM xs f = foldr (&?) (return Match) $ f <$> xs

-- A memoization of matches
type Classification = Map (Rect, Label) Match
  
-- An unchanging context for matching in a matrix
data Context = Context {size :: Size,
                        matrix :: Map Coord Char,
                        clauses :: Map Label Expr}
            deriving (Show)

-- A monad for performing matching in a matrix
-- The String is for logging, and the Any for keeping track of new definite matches or non-matches
type Matcher a = WriterT (String, Any) (ReaderT Context (State Classification)) a

-- Helper function for logging
logMsg :: String -> Matcher ()
logMsg message = tell (message, mempty)

-- Does the pattern match? Update all sub-rectangles as needed
matches :: Expr -> Rect -> Matcher Match

matches Border (x, y, 1, 1) = do
  ch <- lift $ asks $ lookup (x, y) . matrix
  return $ case ch of
    Nothing -> Match
    Just _ -> NoMatch
matches Border _ = return NoMatch

matches AnyRect _ = return Match

matches AnyChar (x, y, 1, 1) = do
  ch <- lift $ asks $ lookup (x, y) . matrix
  return $ case ch of
    Nothing -> NoMatch
    Just _ -> Match
matches AnyChar _ = return NoMatch

matches (SomeChar cs) (x, y, 1, 1) = do
  ch <- lift $ asks $ lookup (x, y) . matrix
  return $ case ch of
      Just c -> if c `member` cs then Match else NoMatch
      Nothing -> NoMatch
matches (SomeChar _) _ = return NoMatch

matches (Var label) rect = do
  memoed <- gets $ lookup (rect, label)
  case memoed of
    Just b -> return b
    Nothing -> do
      modify $ insert (rect, label) Unknown
      logMsg $ "Checking " ++ show label ++ " at " ++ show rect ++ "\n"
      Just expr <- lift $ asks $ lookup label . clauses
      match <- matches expr rect
      logMsg $ "Checked  " ++ show label ++ " at " ++ show rect ++ ": " ++ show match ++ "\n"
      tell (mempty, Any $ match /= Unknown)
      modify $ insert (rect, label) match
      return match

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

matches (Not expr) rect = invert <$> matches expr rect

matches (Sized (x1, x2) (y1, y2) exp) r@(x, y, w, h) = do
  let xOk = x1 <= w && case x2 of Nothing -> True; Just x3 -> w <= x3
      yOk = y1 <= h && case y2 of Nothing -> True; Just y3 -> h <= y3
  return (if xOk && yOk then Match else NoMatch) &? case exp of
    Border -> allMatch
    AnyChar -> allMatch
    SomeChar _ -> allMatch
    _ -> matches exp r
  where allMatch = allM [(x+i, y+j, 1, 1) | i <- [0..w-1], j <- [0..h-1]] $
                   matches exp

-- Collect definite matches of Nothing for the given rectangles, possibly looping until no uncertainty remains.
-- Also collect logs for debugging.
matchAllEmpty :: Context -> [Rect] -> ([Rect], String)
matchAllEmpty con rects = flip evalState empty . flip runReaderT con . fmap (\(a,(b,_)) -> (a,b)) . runWriterT $ go rects
  where go :: [Rect] -> Matcher [Rect]
        go currRects = do
          logMsg "Matching...\n"
          modify $ Map.filter (/= Unknown)
          Just expr <- lift $ asks $ lookup Nothing . clauses
          (currMatches, changed) <- listens (getAny . snd) $ mapM (\rect -> (,) rect <$> matches expr rect) currRects
          logMsg $ "Matching complete, change = " ++ show changed ++ "\n"
          if and [match /= Unknown | (_, match) <- currMatches] || not changed
            then return $ [rect | (rect, Match) <- currMatches]
            else do
                 remainingMatches <- go [rect | (rect, Unknown) <- currMatches]
                 return $ [rect | (rect, Match) <- currMatches] ++ remainingMatches
