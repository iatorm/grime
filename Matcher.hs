module Matcher where

import Expression
import Prelude hiding (lookup)
import Data.Map.Strict (Map, empty, lookup, insert)
import qualified Data.Map.Strict as Map (filter, size)
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
data Match = NoMatch
           | Unknown
           | Match
           deriving (Eq,Ord,Show)

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

-- Keep those that match, signal if any was unknown
filterMatch :: (Monad m) => [a] -> (a -> m Match) -> m ([a], Bool)
filterMatch [] _ = return ([], False)
filterMatch (x : xs) p = do
  res <- p x
  (found, unk) <- filterMatch xs p
  return $ case res of
    Match   -> (x:found, unk)
    Unknown -> (found, True)
    NoMatch -> (found, unk)

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
                        anchors :: [Rect],
                        depth :: Int}
            deriving (Show)

-- A monad for performing matching in a matrix
-- The String is for logging, and the Any for keeping track of new definite matches or non-matches
type Matcher a = WriterT (String, Any) (ReaderT Context (State Classification)) a

-- Modify context anchors, increase logging depth
withAnchors :: ([Rect] -> [Rect]) -> Matcher a -> Matcher a
withAnchors f = local $ \con -> con{anchors = f $ anchors con, depth = depth con + 1}

-- Helper function for logging
logMsg :: String -> Matcher ()
logMsg message = do
  level <- asks depth
  tell (replicate level ' ' ++ message, mempty)

-- Possible widths and heights of matching rectangles
-- May give false positives, but never false negatives
sizes :: Expr -> Matcher ([Int], [Int])
sizes expr = do
  numVars <- asks $ Map.size . clauses
  go numVars expr
  where
    go :: Int -> Expr -> Matcher ([Int], [Int])
    go _ Border = return ([1], [1])
    go _ Edge = do
      (maxX, maxY) <- asks size
      return ([0..maxX+2], [0..maxY+2])
    go _ AnyRect = do
      (maxX, maxY) <- asks size
      return ([0..maxX+2], [0..maxY+2])
    go _ AnyChar = return ([1], [1])
    go _ (SomeChar _ _) = return ([1], [1])
    go 0 (Var _ _) = do
      (maxX, maxY) <- asks size
      return ([0..maxX+2], [0..maxY+2])
    go n (Var rot label) = do
      Just expr <- asks $ lookup label . clauses
      go (n-1) $ orient expr rot
    go n (e1 :> e2) = do
      (ws1, hs1) <- go n e1
      (ws2, hs2) <- go n e2
      return (ascSum ws1 ws2, Asc.isect hs1 hs2)
    go n (e1 :^ e2) = do
      (ws1, hs1) <- go n e1
      (ws2, hs2) <- go n e2
      return (Asc.isect ws1 ws2, ascSum hs1 hs2)
    go n (e1 :| e2) = do
      (ws1, hs1) <- go n e1
      (ws2, hs2) <- go n e2
      return (Asc.nub $ Asc.union ws1 ws2, Asc.nub $ Asc.union hs1 hs2)
    go n (e1 :& e2) = do
      (ws1, hs1) <- go n e1
      (ws2, hs2) <- go n e2
      return (Asc.isect ws1 ws2, Asc.isect hs1 hs2)
    go n (e1 :~ e2) = do
      (ws1, hs1) <- go n e1
      (ws2, hs2) <- go n e2
      return (Asc.nub $ Asc.union ws1 ws2, Asc.nub $ Asc.union hs1 hs2)
    go _ (Not _) = do
      (maxX, maxY) <- asks size
      return ([0..maxX+2], [0..maxY+2])
    go n (Sized (x1,x2) (y1,y2) e) = do
      (maxX, maxY) <- asks size
      (ws, hs) <- case e of
        Border -> return ([0..maxX+2], [0..maxY+2])
        AnyChar -> return ([0..maxX+2], [0..maxY+2])
        SomeChar _ _ -> return ([0..maxX+2], [0..maxY+2])
        _ -> go n e
      let wRange = case x2 of Just high -> [x1..high]
                              Nothing -> [x1..]
          hRange = case y2 of Just high -> [y1..high]
                              Nothing -> [y1..]
      return (Asc.isect ws wRange, Asc.isect hs hRange)
    go _ (Grid (x1,x2) (y1,y2) e) = do
      (maxX, maxY) <- asks size
      return ([0..maxX+2], [0..maxY+2]) -- TODO: implement
    go _ (Count (low, high) e) = do
      (maxX, maxY) <- asks size
      return ([0..maxX+2], [0..maxY+2])
    go _ (InContext e) = do
      (maxX, maxY) <- asks size
      return ([0..maxX+2], [0..maxY+2])
    go _ (Anchor n) = do
      anchors <- asks anchors
      (maxX, maxY) <- asks size
      return $ if length anchors > n
               then let (_, _, w, h) = anchors !! n in ([w], [h])
               else ([0..maxX+2], [0..maxY+2])
    go n (Fixed e) = go n e

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
    Just b -> do
      logMsg $ "Lookup   " ++ show rot ++ show label ++ " at " ++ show rect ++ ": " ++ show b ++ "\n"
      return b
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
  let cuts = Asc.isect hsl . reverse . map (h -) $ takeWhile (<= h) hsr
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
            -- Arguments:
            --  a) Have we placed double vertical lines
            --  b) Have we placed double horizontal lines
            --  c) How many vertical lines have we placed
            --  d) How many horizontal lines have we placed
            --  e) List of vertical cell borders (reversed)
            --  f) List of horizontal cell borders (reversed)
            go :: Bool -> Bool -> Int -> Int -> [Int] -> [Int] -> Matcher Match
            go vOverlap hOverlap numV numH vs@(ver:vers) hs@(hor:hors)
              -- More than allowed vertical lines
              | Just n <- x2, numV > n = return NoMatch
              -- More than allowed horizontal lines
              | Just n <- y2, numH > n = return NoMatch
              -- Reached lower right corner
              | ver == x + w, hor == y + h, vOverlap || x1 <= numV, hOverlap || y1 <= numH = return Match
              -- Incomplete grid => extend
              | otherwise = do
                  let vMin = case () of
                        -- About to reach upper bound on vertical lines => go to end
                        _ | x2 == Just (numV + 1) -> x+w
                        -- No upper bound or already seen overlap => advance at least one cell
                          | isNothing x2 || vOverlap -> ver + 1
                        -- Otherwise may stay in place
                          | otherwise -> ver
                      -- Options for new vertical line
                      vCuts = map (ver +) widths `Asc.isect` [vMin .. x+w]
                      hMin = case () of
                        _ | y2 == Just (numH + 1) -> y+h
                          | isNothing y2 || hOverlap -> hor + 1
                          | otherwise -> hor
                      hCuts = map (hor +) heights `Asc.isect` [hMin .. y+h]
                  -- Only consider vertical lines that produce matches on the right margin
                  (vMargin, unkV) <- filterMatch vCuts $ \newVer ->
                    allMatch [(ver, h1, newVer-ver, h2-h1) | (h1, h2) <- zip (tail hs) hs] $ matches expr
                  -- Same for bottom margin
                  (hMargin, unkH) <- filterMatch hCuts $ \newHor ->
                    allMatch [(v1, hor, v2-v1, newHor-hor) | (v1, v2) <- zip (tail vs) vs] $ matches expr
                  -- Same for corner rectangle
                  (pairs, unkP) <- filterMatch [(newV, newH) | numV == numH, newV <- vMargin, newH <- hMargin] $ \(newV, newH) ->
                    matches expr (ver, hor, newV-ver, newH-hor)
                  -- Recurse for all possible choices of new lines
                  found <- anyMatch ([(numV,   numH+1, vs,        newHor:hs) | numV <= numH, ver == x+w, newHor <- hMargin] ++
                                     [(numV+1, numH,   newVer:vs, hs)        | numV >= numH, hor == y+h, newVer <- vMargin] ++
                                     [(numV+1, numH+1, newVer:vs, newHor:hs) | (newVer, newHor) <- pairs]) $ \(newNumV, newNumH, newVers, newHors) ->
                             go (vOverlap || overlap newVers) (hOverlap || overlap newHors) newNumV newNumH newVers newHors
                  return $ if unkV || unkH || unkP then max Unknown found else found
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
  where context = Context {size = size, matrix = matrix, hasBorders = hasBorders, clauses = clauses, anchors = [], depth = 0}
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
