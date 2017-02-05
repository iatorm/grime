module Matcher where

import Expression
import Prelude hiding (lookup)
import Data.Map.Strict (Map, empty, lookup, insert)
import qualified Data.Map.Strict as Map (filter)
import Data.Set (member)
import Data.Monoid (Any(Any), getAny, mempty)
import Data.Maybe (isNothing)
import qualified Data.List.Ordered as Asc (member, isect, union, unionAll, nub)
import Control.Applicative ((<$>), liftA2)
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

anyMatch :: (Monad m) => [a] -> (a -> m Match) -> m Match
anyMatch xs f = foldr (|?) (return NoMatch) $ f <$> xs

allMatch :: (Monad m) => [a] -> (a -> m Match) -> m Match
allMatch xs f = foldr (&?) (return Match) $ f <$> xs

filterMatch :: (Monad m) => [a] -> (a -> m Match) -> m [a]
filterMatch xs p = foldr (\x -> liftA2 (\match -> if match == Match then (x:) else id) $ p x) (return []) xs

-- Sum of two strictly ordered lists
ascSum :: [Int] -> [Int] -> [Int]
ascSum xs = go
  where go (y:ys) = Asc.nub . Asc.union [y + x | x <- xs] $ go ys
        go [] = []

-- A memoization of matches
type Classification = Map (Rect, D4, Label) Match

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

-- Possible widths and heights of matching rectangles
-- May give false positives, but never false negatives
sizes :: Expr -> Matcher ([Int], [Int])
sizes Border = return ([1], [1])
sizes Edge = do
  (maxX, maxY) <- asks size
  return ([0..maxX+2], [0..maxY+2])
sizes AnyRect = do
  (maxX, maxY) <- asks size
  return ([0..maxX+2], [0..maxY+2])
sizes AnyChar = return ([1], [1])
sizes (SomeChar _ _) = return ([1], [1])
sizes (Var _ _) = do
  (maxX, maxY) <- asks size
  return ([0..maxX+2], [0..maxY+2]) -- TODO: implement
sizes (e1 :> e2) = do
  (ws1, hs1) <- sizes e1
  (ws2, hs2) <- sizes e2
  return (ascSum ws1 ws2, Asc.isect hs1 hs2)
sizes (e1 :^ e2) = do
  (ws1, hs1) <- sizes e1
  (ws2, hs2) <- sizes e2
  return (Asc.isect ws1 ws2, ascSum hs1 hs2)
sizes (e1 :| e2) = do
  (ws1, hs1) <- sizes e1
  (ws2, hs2) <- sizes e2
  return (Asc.nub $ Asc.union ws1 ws2, Asc.nub $ Asc.union hs1 hs2)
sizes (e1 :& e2) = do
  (ws1, hs1) <- sizes e1
  (ws2, hs2) <- sizes e2
  return (Asc.isect ws1 ws2, Asc.isect hs1 hs2)
sizes (e1 :~ e2) = do
  (ws1, hs1) <- sizes e1
  (ws2, hs2) <- sizes e2
  return (Asc.nub $ Asc.union ws1 ws2, Asc.nub $ Asc.union hs1 hs2)
sizes (Not _) = do
  (maxX, maxY) <- asks size
  return ([0..maxX+2], [0..maxY+2])
sizes (Sized (x1,x2) (y1,y2) e) = do
  (maxX, maxY) <- asks size
  (ws, hs) <- case e of
    Border -> return ([0..maxX+2], [0..maxY+2])
    AnyChar -> return ([0..maxX+2], [0..maxY+2])
    SomeChar _ _ -> return ([0..maxX+2], [0..maxY+2])
    _ -> sizes e
  return (Asc.isect ws wRange, Asc.isect hs hRange)
  where wRange = case x2 of Just high -> [x1..high]
                            Nothing -> [x1..]
        hRange = case y2 of Just high -> [y1..high]
                            Nothing -> [y1..]
sizes (Grid (x1,x2) (y1,y2) e) = do
  (maxX, maxY) <- asks size
  return ([0..maxX+2], [0..maxY+2]) -- TODO: implement
sizes (Count (low, high) e) = do
  (maxX, maxY) <- asks size
  return ([0..maxX+2], [0..maxY+2])
sizes (InContext e) = do
  (maxX, maxY) <- asks size
  return ([0..maxX+2], [0..maxY+2])
sizes (Anchor n) = do
  anchors <- asks anchors
  (maxX, maxY) <- asks size
  return $ if length anchors > n
           then let (_, _, w, h) = anchors !! n in ([w], [h])
           else ([0..maxX+2], [0..maxY+2])
sizes (Fixed e) = sizes e

-- Does the pattern match? Update all sub-rectangles as needed
matches :: Expr -> Rect -> Matcher Match

matches Border (x, y, 1, 1) = do
  ch <- lift $ asks $ lookup (x, y) . matrix
  return $ case ch of
    Nothing -> Match
    Just _ -> NoMatch
matches Border _ = return NoMatch

matches Edge (x, y, w, h) = do
  (maxX, maxY) <- asks size
  return $ if (x == 0 || x == maxX) && w == 0 && y >= 0 && y+h <= maxY ||
              (y == 0 || y == maxY) && h == 0 && x >= 0 && x+w <= maxX
           then Match
           else NoMatch

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

matches (Var rot label) rect = do
  memoed <- gets $ lookup (rect, rot, label)
  case memoed of
    Just b -> return b
    Nothing -> do
      modify $ insert (rect, rot, label) Unknown
      logMsg $ "Checking " ++ show rot ++ show label ++ " at " ++ show rect ++ "\n"
      Just expr <- lift $ asks $ lookup label . clauses
      match <- withAnchors (const []) $ matches (orient expr rot) rect
      logMsg $ "Checked  " ++ show rot ++ show label ++ " at " ++ show rect ++ ": " ++ show match ++ "\n"
      tell (mempty, Any $ match /= Unknown)
      modify $ insert (rect, rot, label) match
      return match

matches (lExp :> rExp) (x, y, w, h) = do
  (wsl, hsl) <- sizes lExp
  (wsr, hsr) <- sizes rExp
  let cuts = Asc.isect wsl . reverse . map (w -) $ takeWhile (<= w) wsr
  if h `Asc.member` Asc.isect hsl hsr
    then anyMatch cuts $ \i -> matches lExp (x, y, i, h) &? matches rExp (x+i, y, w-i, h)
    else return NoMatch

matches (tExp :^ bExp) (x, y, w, h) = do
  (wsl, hsl) <- sizes tExp
  (wsr, hsr) <- sizes bExp
  let cuts = Asc.isect hsl . reverse . map (h -) $ takeWhile (<= h) hsl
  if w `Asc.member` Asc.isect wsl wsr
    then anyMatch cuts $ \j -> matches tExp (x, y, w, j) &? matches bExp (x, y+j, w, h-j)
    else return NoMatch
          
matches (exp1 :| exp2) rect = matches exp1 rect |? matches exp2 rect

matches (exp1 :& exp2) rect = matches exp1 rect &? matches exp2 rect

matches (exp1 :~ exp2) rect = do
  first <- matches exp1 rect
  second <- matches exp2 rect
  return $ case (first, second) of
    (Unknown, _) -> Unknown
    (_, Unknown) -> Unknown
    _            -> if first /= second then Match else NoMatch

matches (Not expr) rect = invert <$> matches expr rect

matches (Sized (x1, x2) (y1, y2) expr) r@(x, y, w, h) = do
  let xOk = x1 <= w && case x2 of Nothing -> True; Just x3 -> w <= x3
      yOk = y1 <= h && case y2 of Nothing -> True; Just y3 -> h <= y3
  return (if xOk && yOk then Match else NoMatch) &? case expr of
    Border -> allCells
    AnyChar -> allCells
    SomeChar _ _ -> allCells
    _ -> matches expr r
  where allCells = allMatch [(x+i, y+j, 1, 1) | i <- [0..w-1], j <- [0..h-1]] $
                   matches expr

matches (Grid (0, _) _ _) (_, _, 0, _) = return Match
matches (Grid _ (0, _) _) (_, _, _, 0) = return Match
matches (Grid xr@(x1, x2) yr@(y1, y2) expr) r@(x, y, w, h) = do
  (ws, hs) <- sizes expr
  goWith ws hs
  where goWith widths heights = go False False 0 0 [x] [y]
          where
            go :: Bool -> Bool -> Int -> Int -> [Int] -> [Int] -> Matcher Match
            go hOverlap vOverlap numH numV hs@(hor:hors) vs@(ver:vers)
              | Just n <- x2, numH > n = return NoMatch
              | Just n <- y2, numV > n = return NoMatch
              | hor == x + w, ver == y + h, hOverlap || x1 <= numH, vOverlap || y1 <= numV = return Match
              | otherwise = do
                  let hMin = case () of
                        _ | x2 == Just (numH + 1) -> x+w
                          | isNothing x2 || hOverlap -> hor + 1
                          | otherwise -> hor
                      hCuts = map (hor +) widths `Asc.isect` [hMin .. x+w]
                      vMin = case () of
                        _ | y2 == Just (numV + 1) -> y+h
                          | isNothing y2 || vOverlap -> ver + 1
                          | otherwise -> ver
                      vCuts = map (ver +) heights `Asc.isect` [vMin .. y+h]
                  hMargin <- filterMatch hCuts $ \newHor ->
                    allMatch [(hor, v1, newHor-hor, v2-v1) | (v1, v2) <- zip (tail vs) vs] $ matches expr
                  vMargin <- filterMatch vCuts $ \newVer ->
                    allMatch [(h1, ver, h2-h1, newVer-ver) | (h1, h2) <- zip (tail hs) hs] $ matches expr
                  pairs <- filterMatch [(newH, newV) | numH == numV, newH <- hMargin, newV <- vMargin] $ \(newH, newV) ->
                    matches expr (hor, ver, newH-hor, newV-ver)
                  anyMatch ([(numH, numV+1, hs, newVer:vs) | numH <= numV, hor == x+w, newVer <- vMargin] ++
                            [(numH+1, numV, newHor:hs, vs) | numH >= numV, ver == y+h, newHor <- hMargin] ++
                            [(numH+1, numV+1, newHor:hs, newVer:vs) | (newHor, newVer) <- pairs]) $ \(newNumH, newNumV, newHors, newVers) ->
                    go (hOverlap || overlap newHors) (vOverlap || overlap newVers) newNumH newNumV newHors newVers
            overlap (a:b:c) = a == b
            overlap _ = False

matches (Count (low, high) expr) (x, y, w, h) = do
  (ws, hs) <- sizes expr
  let allRects = [(x', y', w', h') |
                  w' <- Asc.isect ws [0..w], h' <- Asc.isect hs [0..h],
                  x' <- [x..x+w-w'], y' <- [y..y+h-h']]
      total = length allRects
  go 0 0 total allRects
  where go :: Int -> Int -> Int -> [Rect] -> Matcher Match
        go found unk left _ | found >= low, Nothing <- high = return Match
                            | found >= low, Just n <- high, n >= found + left + unk = return Match
                            | found + unk + left < low = return NoMatch
                            | found + left < low = return Unknown
                            | Just n <- high, found > n = return NoMatch
        go found unk left [] = error "Unreachable state."
        go found unk left (r:rs) = do
          matchR <- matches expr r
          case matchR of
            Match -> go (found+1) unk (left-1) rs
            NoMatch -> go found unk (left-1) rs
            Unknown -> go found (unk+1) (left-1) rs

matches (InContext expr) r@(x, y, w, h) = do
  (ws, hs) <- sizes expr
  (maxX', maxY') <- asks size
  addBorders <- asks hasBorders
  let (minX, maxX, minY, maxY) = if addBorders
                                 then (-1, maxX'+1, -1, maxY'+1)
                                 else ( 0, maxX',    0, maxY')
      surrounding = [(x', y', w', h') |
                     w' <- Asc.isect ws [w .. maxX - minX],
                     h' <- Asc.isect hs [h .. maxY - minY],
                     x' <- [max minX $ x + w - w' .. min x $ maxX - w'],
                     y' <- [max minY $ y + h - h' .. min y $ maxY - h']]
  withAnchors (r:) $ anyMatch surrounding $ matches expr

matches (Anchor n) r = do
  anchs <- asks anchors
  return $ if anchs !! n == r then Match else NoMatch

matches (Fixed e) r = matches e r

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
        selfAndMatch rect = (,) rect <$> matches (Var (Rot 0) Nothing) rect
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
