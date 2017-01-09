module Matcher where

import Expression
import Prelude hiding (lookup)
import Data.Map.Strict (Map, empty, lookup, insert)
import qualified Data.Map.Strict as Map (filter)
import Data.Set (member)
import Data.Monoid (Any(Any), getAny, mempty)
import Control.Applicative ((<$>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Lazy (WriterT, tell, listens, runWriterT)
import Control.Monad.Reader (ReaderT, asks, reader, local, runReaderT)
import Control.Monad.State.Lazy (State, gets, modify, evalState)

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

-- A changing context for matching in a matrix
data Context = Context {size :: Size,
                        matrix :: Map Coord Char,
                        clauses :: Map Label Expr,
                        hasBorders :: Bool,
                        anchors :: [Rect]}
            deriving (Show)

-- A monad for performing matching in a matrix
-- The String is for logging, and the Any for keeping track of new definite matches or non-matches
type Matcher a = WriterT (String, Any) (ReaderT Context (State Classification)) a

-- Modify context anchors
withAnchors :: ([Rect] -> [Rect]) -> Matcher a -> Matcher a
withAnchors f = local $ \con -> con{anchors = f $ anchors con}

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

matches (SomeChar isPos cs) (x, y, 1, 1) = do
  ch <- lift $ asks $ lookup (x, y) . matrix
  return $ if (ch `member` cs) == isPos then Match else NoMatch
matches (SomeChar _ _) _ = return NoMatch

matches (Var label) rect = do
  memoed <- gets $ lookup (rect, label)
  case memoed of
    Just b -> return b
    Nothing -> do
      modify $ insert (rect, label) Unknown
      logMsg $ "Checking " ++ show label ++ " at " ++ show rect ++ "\n"
      Just expr <- lift $ asks $ lookup label . clauses
      match <- withAnchors (const []) $ matches expr rect
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

matches (exp1 :~ exp2) rect = do
  first <- matches exp1 rect
  second <- matches exp2 rect
  return $ case (first, second) of
    (Unknown, _) -> Unknown
    (_, Unknown) -> Unknown
    _            -> if first == second then Match else NoMatch

matches (Not expr) rect = invert <$> matches expr rect

matches (Sized (x1, x2) (y1, y2) expr) r@(x, y, w, h) = do
  let xOk = x1 <= w && case x2 of Nothing -> True; Just x3 -> w <= x3
      yOk = y1 <= h && case y2 of Nothing -> True; Just y3 -> h <= y3
  return (if xOk && yOk then Match else NoMatch) &? case expr of
    Border -> allMatch
    AnyChar -> allMatch
    SomeChar _ _ -> allMatch
    _ -> matches expr r
  where allMatch = allM [(x+i, y+j, 1, 1) | i <- [0..w-1], j <- [0..h-1]] $
                   matches expr

matches (InContext expr) r@(x, y, w, h) = do
  (maxX', maxY') <- asks size
  addBorders <- asks hasBorders
  let (minX, maxX, minY, maxY) = if addBorders
                                 then (-1, maxX'+1, -1, maxY'+1)
                                 else ( 0,   maxX',  0,   maxY')
      surrounding = [(x', y', w', h') | x' <- [0..x], y' <- [0..y],
                                        w' <- [w+x-x'..maxX-x'], h' <- [h+y-y'..maxY-y']]
  withAnchors (++[r]) $ anyM surrounding $ matches expr

matches (Anchor n) r = do
  anchs <- asks anchors
  return $ if anchs !! n == r then Match else NoMatch

-- Collect definite matches of Nothing for the given rectangles, possibly looping until no uncertainty remains.
-- Also collect logs for debugging.
matchAllEmpty :: Size -> Bool -> Map Coord Char -> Map Label Expr -> [Rect] -> ([Rect], String)
matchAllEmpty size hasBorders matrix clauses rects =
  flip evalState empty .
  flip runReaderT context .
  fmap (\(a,(b,_)) -> (a,b)) .
  runWriterT $
  go rects
  where context = Context {size = size, matrix = matrix, hasBorders = hasBorders, clauses = clauses, anchors = []}
        selfAndMatch rect = (,) rect <$> matches (Var Nothing) rect
        go :: [Rect] -> Matcher [Rect]
        go currRects = do
          logMsg "Matching...\n"
          modify $ Map.filter (/= Unknown)
          (currMatches, changed) <- listens (getAny . snd) $ mapM selfAndMatch currRects
          logMsg $ "Matching complete, change = " ++ show changed ++ "\n"
          if and [match /= Unknown | (_, match) <- currMatches] || not changed
            then return $ [rect | (rect, Match) <- currMatches]
            else do
                 remainingMatches <- go [rect | (rect, Unknown) <- currMatches]
                 return $ [rect | (rect, Match) <- currMatches] ++ remainingMatches
