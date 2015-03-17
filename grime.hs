-- A two-dimensional language based on conjunctive grammars

import Prelude hiding (lookup)
import Data.Map.Strict (Map, empty, lookup, insert, fromList, toList)
import Data.Set (Set, member, singleton, fromAscList, toAscList)
import Control.Monad (forM, forM_, liftM2, (>=>), filterM, when)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State.Lazy (State, gets, modify, evalState)
import Text.Parsec (Parsec, ParseError, parse, try, (<?>), (<|>), between)
import Text.Parsec.Char (char, oneOf, noneOf, anyChar, string)
import Text.Parsec.Expr
import System.Environment (getArgs)

-- The label of a variable
type Label = String

type Coord = (Int, Int) -- x, y
type Size = (Int, Int) -- w, h
type Rect = (Int, Int, Int, Int) -- x, y, w, h

-- An expression that may or may not match a rectangle of characters
data Expr = Empty -- Mathces a 0xn or nx0 rectangle
          | Border -- Matches the rectangle border symbol
          | AnyChar -- Matches any single character (not border)
          | SomeChar (Set Char) -- Matches any of the given characters
          | Var Label -- Matches the given variable
          | Expr :> Expr -- Horizontal concatenation
          | HPlus Expr -- Horizontal repetition
          | Expr :^ Expr -- Vertical concatenation
          | VPlus Expr -- Vertical repetition
          | Expr :| Expr -- Disjunction
          | Expr :& Expr -- Conjunction
          | Not Expr -- Negation

instance Show Expr where
  show Empty = "_"
  show Border = "$"
  show AnyChar = "."
  show (SomeChar set) = "[" ++ toAscList set ++ "]"
  show (Var label) = label
  show (e1 :> e2) = show e1 ++ show e2
  show (HPlus e) = "(" ++ show e ++ ")+"
  show (e1 :^ e2) = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"
  show (VPlus e) = "(" ++ show e ++ ")/+"
  show (e1 :| e2) = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
  show (e1 :& e2) = "(" ++ show e1 ++ "&" ++ show e2 ++ ")"
  show (Not e) = "(" ++ show e ++ ")!"

-- Expression parser
pExpr :: Parsec String () Expr
pExpr = buildExpressionParser opTable term <?> "expression"
  where term = try (parens pExpr) <|> try escaped <|> reserved <?> "term"
        escaped = do
          char '\\'
          c <- anyChar
          return $ SomeChar $ singleton c
        reserved = do
          c <- oneOf ("_$.dulans" ++ ['A'..'Z'])
          return $ case c of
            '_' -> Empty
            '$' -> Border
            '.' -> AnyChar
            'd' -> SomeChar $ fromAscList ['0'..'9']
            'u' -> SomeChar $ fromAscList ['A'..'Z']
            'l' -> SomeChar $ fromAscList ['a'..'z']
            'a' -> SomeChar . fromAscList $ ['A'..'Z'] ++ ['a'..'z']
            'n' -> SomeChar . fromAscList $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
            's' -> SomeChar . fromAscList $ ['!'..'/'] ++ [':'..'@'] ++ ['['..'`'] ++ ['{'..'~']
            _ -> Var [c]
        parens = char '(' `between` char ')'
        opTable = [[Postfix (char '?' >> return (Empty :|))],
                   [Postfix (char '+' >> return HPlus)],
                   [Postfix (char '*' >> return (\e -> Empty :| HPlus e))],
                   [Postfix (try (string "/+") >> return VPlus)],
                   [Postfix (try (string "/*") >> return (\e -> Empty :| VPlus e))],
                   [Postfix (char '!' >> return Not)],
                   [Postfix (char '#' >> return contains)],
                   [Infix (return (:>)) AssocLeft],
                   [Infix (char '/' >> return (:^)) AssocLeft],
                   [Infix (char '&' >> return (:&)) AssocLeft],
                   [Infix (char '|' >> return (:|)) AssocLeft]]
        contains expr = rect :^ (rect :> expr :> rect) :^ rect
          where rect = Empty :| (HPlus $ VPlus AnyChar)

-- File parser
pLines :: String -> Either ParseError (Map Label Expr)
pLines s = fmap fromList . mapM parseEq $ lines s
  where parseEq e@('\\':'=':_) = do
          expr <- parse pExpr "" e
          return ("", expr)
        parseEq (l:'=':e) = do
          expr <- parse pExpr "" e
          return ([l], expr)
        parseEq e = do
          expr <- parse pExpr "" e
          return ("", expr)
  
-- An unchanging context for matching in a matrix
data Context = Context {size :: Size,
                        matrix :: Map Coord Char,
                        clauses :: Map Label Expr}
            deriving (Show)

-- Parse a matrix from newline-delimited string
mkMatrix :: String -> (Size, Map Coord Char)
mkMatrix s = ((w, h), fromList pairs)
  where rows = lines s
        pairs = [((x, y), c) | (y, row) <- zip [0..] rows, (x, c) <- zip [0..] row]
        w = maximum $ map length rows
        h = length rows

-- A memoization of matches
type Classification = Map (Rect, Label) Bool

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

-- Does the pattern match? Update all sub-rectangles as needed
matches :: Expr -> Rect -> Matcher Bool
matches Empty (_, _, w, h) = return $ w == 0 || h == 0
matches Border (x, y, 1, 1) = do
  ch <- asks $ lookup (x, y) . matrix
  return $ case ch of
    Nothing -> True
    Just _ -> False
matches Border _ = return False
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
  case  memoed of
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



-- Collect matches of "" for the given rectangles.
matchAllEmpty :: Context -> [Rect] -> [Rect]
matchAllEmpty con rects = flip evalState empty . flip runReaderT con $ do
  Just expr <- asks $ lookup "" . clauses
  filterM (matches expr) rects

-- Take a submatrix of a newline-delimited string
submatrix :: Rect -> String -> String
submatrix (x, y, w, h) = unlines . take h . drop y . map (take w . drop x) . lines

main :: IO ()
main = do
  args <- getArgs
  let (opts, grFile, matFile) = case args of
        ['-':a, b, c] -> (a, b, c)
        [a, b] -> ("", a, b)
  parsedGrammar <- fmap pLines $ readFile grFile
  case parsedGrammar of
    Left error -> print error
    Right grammar -> do
      pict <- readFile matFile
      let (sze@(wMat, hMat), mat) = mkMatrix pict
          matches = (if elem 'a' opts || elem 'n' opts then id else take 1) .
                    matchAllEmpty (Context sze mat grammar) $
                   if elem 'e' opts
                   then [(0, 0, wMat, hMat)]
                   else [(x, y, w, h) |
                         x <- [-1..wMat], y <- [-1..hMat],
                         w <- [0..wMat+1-x], h <- [0..hMat+1-y]]
      when (elem 'd' opts) $ do
        print sze
        forM_ (toList grammar) $ \(l, e) ->
          putStrLn $ (if l==[] then "Pat = " else l ++ " = ") ++ show e
      if (elem 'n' opts /= elem 'e' opts)
        then print $ length matches
        else forM_ matches $ \rect -> do
        when (elem 'p' opts) $ print rect
        when (not $ elem 's' opts) . putStrLn $ submatrix rect pict