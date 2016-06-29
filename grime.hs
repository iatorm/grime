-- A two-dimensional language based on Boolean grammars

import Prelude hiding (lookup)
import Data.List (nub, (\\), sort)
import Data.Map.Strict (Map, empty, lookup, insert, fromList, toList)
import Data.Set (Set, member, singleton, fromAscList, toAscList)
import Control.Monad (forM, forM_, liftM2, (>=>), filterM, when)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State.Lazy (State, gets, modify, evalState)
import Text.Parsec (Parsec, ParseError, parse, try, (<?>), (<|>), between, many, manyTill, many1, choice, optionMaybe)
import Text.Parsec.Char (char, oneOf, noneOf, anyChar, string, upper, lower, digit)
import Text.Parsec.Expr
import System.Environment (getArgs)

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

mkSomeChar :: String -> Expr
mkSomeChar = SomeChar . fromAscList . sort

-- Expression parser
pExpr :: Parsec String () Expr
pExpr = buildExpressionParser opTable term <?> "expression"
  where term = choice (map try
               [parens pExpr, escaped, charClass, reserved]) <?> "term"
        escaped = do
          char '\\'
          c <- anyChar
          return $ mkSomeChar [c]
        flat = Sized (0, Nothing) (0, Just 0) AnyRect
        thin = Sized (0, Just 0) (0, Nothing) AnyRect
        reserved = do
          c <- oneOf ("_$.ftbdulans" ++ ['A'..'Z'])
          return $ case c of
            '_' -> flat :| thin
            'b' -> Border
            '$' -> AnyRect
            '.' -> AnyChar
            'f' -> flat
            't' -> thin
            'd' -> mkSomeChar ['0'..'9']
            'u' -> mkSomeChar ['A'..'Z']
            'l' -> mkSomeChar ['a'..'z']
            'a' -> mkSomeChar $ ['A'..'Z'] ++ ['a'..'z']
            'n' -> mkSomeChar $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
            's' -> mkSomeChar $ ['!'..'/'] ++ [':'..'@'] ++ ['['..'`'] ++ ['{'..'~']
            _ -> Var $ Just c
        parens = char '(' `between` char ')'
        classLetter = noneOf "[]-,\\" <|> (char '\\' >> oneOf "[]-,\\")
        classRange = do
          a <- classLetter
          char '-'
          b <- classLetter
          return [a..b]
        charClass = between (char '[') (char ']') $ do
          pos <- many $ try classRange <|> fmap (:[])classLetter
          neg <- optionMaybe $ char ',' >> (many $ try classRange <|> fmap (:[]) classLetter)
          return $ case (null pos, neg) of
            (True, Nothing) -> AnyChar
            (False, Nothing) -> mkSomeChar $ concat pos
            (True, Just negs) -> AnyChar :& (Not . mkSomeChar $ concat negs)
            (False, Just negs) -> mkSomeChar $ concat pos \\ concat negs
        numRange = do
          lower <- many digit
          maybeDash <- optionMaybe $ char '-'
          upper <- many digit
          let lowerNum = if null lower then 0 else read lower
              upperNum = if null upper then Nothing else Just $ read upper
          return $ case maybeDash of
            Nothing -> (lowerNum, if null lower then Nothing else Just lowerNum)
            Just _ -> (lowerNum, upperNum)
        sizeRange = between (char '{') (char '}') $ do
          xRange <- numRange
          maybeYRange <- optionMaybe $ char ',' >> numRange
          return $ case maybeYRange of
            Nothing -> Sized xRange xRange
            Just yRange -> Sized xRange yRange
        opTable = [[Postfix postfix],
                   [Infix (return (:>)) AssocLeft],
                   [Infix (char '/' >> return (:^)) AssocLeft],
                   [Infix (char ' ' >> return (:>)) AssocLeft],
                   [Infix (char '&' >> return (:&)) AssocLeft],
                   [Infix (char '|' >> return (:|)) AssocLeft]]
        postfix = fmap (foldr1 (.) . reverse) . many1 . choice . map try $
          [sizeRange,
           char '?' >> return (thin :|),
           string "/?" >> return (flat :|),
           char '+' >> return HPlus,
           char '*' >> return (\e -> thin :| HPlus e),
           string "/+" >> return VPlus,
           string "/*" >> return (\e -> flat :| VPlus e),
           char '!' >> return Not,
           char '#' >> return contains]
        contains expr = (flat :| rect) :^ ((thin :| rect) :> expr :> (thin :| rect)) :^ (flat :| rect)
          where rect = HPlus $ VPlus AnyChar


-- File parser
pLines :: String -> Either ParseError (String, Map Label Expr)
pLines = fmap foldTuples . mapM (parse pLine "") . lines
  where foldTuples = foldr (\(a,(b1,b2)) (c,d) -> (a++c, insert b1 b2 d)) ("", empty)
        pLine = try parseOptionLine <|> fmap (\e -> ("", e)) parseLine
        parseOptionLine = do
          os <- oneOf "enapsbd" `manyTill` char '`'
          e <- parseLine
          return (os, e)
        parseLine = try parseDef <|> fmap (\e -> (Nothing, e)) pExpr
        parseDef = do
          label <- upper
          char '='
          e <- pExpr
          return (Just label, e)
  
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

-- Take a submatrix of a newline-delimited string
submatrix :: Rect -> String -> String
submatrix (x, y, w, h) = unlines . take h . drop y . map (take w . drop x) . lines

main :: IO ()
main = do
  args <- getArgs
  let (cmdOpts, grFile, matFile) = case args of
        ['-':a, b, c] -> (a, b, c)
        [a, b] -> ("", a, b)
  parsedGrammar <- fmap pLines $ readFile grFile
  case parsedGrammar of
    Left error -> print error
    Right (fileOpts, grammar) -> do
      pict <- readFile matFile
      let opts = [o | o <- nub $ cmdOpts ++ fileOpts, elem o cmdOpts /= elem o fileOpts]
          (sze@(wMat, hMat), mat) = mkMatrix pict
          (minX, minY, numX, numY) = if elem 'b' opts then (-1, -1, wMat+1, hMat+1) else (0, 0, wMat, hMat)
          matches = (if elem 'a' opts || elem 'n' opts then id else take 1) .
                    matchAllEmpty (Context sze mat grammar) $
                    if elem 'e' opts
                    then [(minX, minY, numX, numY)]
                    else [(x, y, w, h) |
                          w <- [0..numX], h <- [0..numY],
                          x <- [minX..numX-w], y <- [minY..numY-h]]
      when (elem 'd' opts) $ do
        putStrLn opts
        print sze
        forM_ (toList grammar) $ \(l, e) ->
          putStrLn $ (case l of Nothing -> "Pat = "; Just a -> a:" = ") ++ show e
      if (elem 'n' opts /= elem 'e' opts)
        then print $ length matches
        else forM_ matches $ \rect -> do
        when (elem 'p' opts) $ print rect
        when (not $ elem 's' opts) . putStrLn $ submatrix rect pict
