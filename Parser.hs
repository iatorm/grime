module Parser (parseGrFile, parseMatFile, validOpts) where

import Matcher
import Data.Maybe (catMaybes)
import Data.List ((\\), sort)
import Data.Set (fromAscList)
import Data.Map.Strict (Map, empty, insert, fromList)
import Text.Parsec (Parsec, ParseError, parse, try, (<?>), (<|>), between, many, manyTill, many1, choice, optionMaybe, sepEndBy)
import Text.Parsec.Char (char, oneOf, noneOf, anyChar, string, upper, digit, endOfLine)
import Text.Parsec.Expr

-- Usable option characters
validOpts = "abdenps"

-- Special expressions
flat = Sized (0, Nothing) (0, Just 0) AnyRect
thin = Sized (0, Just 0) (0, Nothing) AnyRect

-- Make a finite character class from string
mkSomeChar :: String -> Expr
mkSomeChar = SomeChar . fromAscList . sort

-- Single-character expressions
reservedChars = [('$', AnyRect),
                 ('.', AnyChar),
                 ('f', flat),
                 ('t', thin),
                 ('_', flat :| thin),
                 ('b', Border),
                 ('d', mkSomeChar ['0'..'9']),
                 ('u', mkSomeChar ['A'..'Z']),
                 ('l', mkSomeChar ['a'..'z']),
                 ('a', mkSomeChar $ ['A'..'Z'] ++ ['a'..'z']),
                 ('n', mkSomeChar $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']),
                 ('s', mkSomeChar $ ['!'..'/'] ++ [':'..'@'] ++ ['['..'`'] ++ ['{'..'~'])]

-- Parse an escaped literal
escapedLit :: Parsec String () Expr
escapedLit = do
  char '\\'
  c <- anyChar
  return $ mkSomeChar [c]

-- Parse a nonterminal
nonterm :: Parsec String () Expr
nonterm = do
  label <- upper
  return $ Var $ Just label

-- Parse a reserved character
reserved :: Parsec String () Expr
reserved = do
  char <- oneOf $ fst <$> reservedChars
  let Just expr = lookup char reservedChars
  return expr

-- Parse a character class
charClass :: Parsec String () Expr
charClass = char '[' `between` char ']' $ do
  include <- many $ try classRange <|> pure <$> classChar
  maybeExclude <- optionMaybe $ char ',' >> (many $ try classRange <|> pure <$> classChar)
  return $ case (null include, maybeExclude) of
    (True, Nothing) -> AnyChar
    (False, Nothing) -> mkSomeChar $ concat include
    (True, Just exclude) -> AnyChar :& (Not . mkSomeChar $ concat exclude)
    (False, Just exclude) -> mkSomeChar $ concat include \\ concat exclude
  where needsEscape = "[]-,\\"
        classChar = noneOf needsEscape <|> (char '\\' >> oneOf needsEscape)
        classRange = do
          a <- classChar
          char ','
          b <- classChar
          return [a..b]

-- Parse a numeric range
numRange :: Parsec String () Range
numRange = do
  lower <- many digit
  maybeDash <- optionMaybe $ char '-'
  upper <- many digit
  let lowerNum = if null lower then 0 else read lower
      upperNum = if null upper then Nothing else Just $ read upper
  return $ case maybeDash of
    Nothing -> (lowerNum, if null lower then Nothing else Just lowerNum)
    Just _ -> (lowerNum, upperNum)

-- Parse a size constraint
sizeConstr :: Parsec String () (Expr -> Expr)
sizeConstr = char '{' `between` char '}' $ do
  xRange <- numRange
  maybeYRange <- optionMaybe $ char ',' >> numRange
  return $ case maybeYRange of
    Nothing -> Sized xRange xRange
    Just yRange -> Sized xRange yRange

-- Parse an expression
expression :: Parsec String () Expr
expression = buildExpressionParser opTable term <?> "expression"
  where term = parens expression <|> nonterm <|> reserved <|> escapedLit <|> charClass <?> "term"
        parens = char '(' `between` char ')'
        opTable = [[Postfix postfix],
                   [Infix (return (:>)) AssocLeft],
                   [Infix (char '/' >> return (:^)) AssocLeft],
                   [Infix (char ' ' >> return (:>)) AssocLeft],
                   [Infix (char '&' >> return (:&)) AssocLeft],
                   [Infix (char '|' >> return (:|)) AssocLeft]]
        postfixes = [sizeConstr,
                     char '?' >> return (thin :|),
                     try (string "/?") >> return (flat :|),
                     char '+' >> return HPlus,
                     try (string "/+") >> return VPlus,
                     char '*' >> return (\e -> thin :| HPlus e),
                     try (string "/*") >> return (\e -> flat :| VPlus e),
                     char '!' >> return Not,
                     char '#' >> return contains]
        postfix = do
          operations <- many1 $ choice postfixes
          return $ foldr1 (flip (.)) operations
        contains expr = (flat :| rect) :^ ((thin :| rect) :> expr :> (thin :| rect)) :^ (flat :| rect)
          where rect = HPlus $ VPlus AnyChar

-- Parse a line of a grammar file into options, label, and expression
contentLine :: Parsec String () (String, (Label, Expr))
contentLine = do
  opts <- try (oneOf validOpts `manyTill` char '`') <|> return ""
  label <- optionMaybe $ try (upper <* char '=')
  expr <- expression
  return (opts, (label, expr))

-- Parse a comment
commentLine :: Parsec String () ()
commentLine = do
  char '|'
  many $ noneOf "\n\r"
  return ()

-- Parse either a content line or a comment
grammarLine :: Parsec String () (Maybe (String, (Label, Expr)))
grammarLine = try (commentLine >> return Nothing) <|> (Just <$> contentLine)

-- File parser
parseGrFile :: String -> Either ParseError (String, Map Label Expr)
parseGrFile grammar = foldToMap <$> contents
  where contents :: Either ParseError [(String, (Label, Expr))]
        contents = catMaybes <$> parse (grammarLine `sepEndBy` endOfLine) "grammar file" grammar
        foldToMap triples = (firsts, folded)
          where firsts = concat $ fst <$> triples
                folded = foldr (\(label, expr) assoc -> insert label expr assoc) empty $ snd <$> triples

-- Parse a matrix from newline-delimited string
parseMatFile :: String -> (Size, Map Coord Char)
parseMatFile s = ((w, h), fromList pairs)
  where rows = lines s
        pairs = [((x, y), c) | (y, row) <- zip [0..] rows, (x, c) <- zip [0..] row]
        w = maximum $ map length rows
        h = length rows
