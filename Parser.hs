module Parser (Option(..), parseGrFile, parseMatFile, parseOptions) where

import Expression
import PrattParser
import Data.Maybe (catMaybes)
import Data.List ((\\), sort, nub)
import Data.Set (fromAscList)
import Data.Map.Strict (Map, empty, insert, fromList)
import Control.Applicative((<$>), (<*), pure)
import Text.Parsec (Parsec, ParseError, parse, runParser, try, (<?>), (<|>), between, many, manyTill, many1,
                    choice, optionMaybe, sepEndBy, notFollowedBy, lookAhead, eof, getState, putState)
import Text.Parsec.Char (char, oneOf, noneOf, anyChar, string, upper, digit)

-- Command-line and grammar options
data Option = Entire
            | Number
            | AllMatches
            | Positions
            | Patterns
            | AddBorder
            | Debug0
            | Debug1
            deriving (Show, Eq)

-- Parse a string of options
options :: Parsec String () [Option]
options = concat <$> many (choice option) <?> "option string"
  where option = [char 'e' >> return [Entire],
                  char 'n' >> return [Number],
                  char 'a' >> return [AllMatches],
                  char 'p' >> return [Positions],
                  char 's' >> return [Patterns],
                  char 'b' >> return [AddBorder],
                  try $ string "d1" >> return [Debug0, Debug1],
                  char 'd' >> optionMaybe (char '0') >> return [Debug0]]

-- Parse an end of line
endOfLine :: Parsec String s String
endOfLine = try (string "\r\n") <|> (pure <$> oneOf "\n\r") <?> "end of line"

-- Parse a string with quotes, return a string without them
quoted :: Parsec String Bool String
quoted = concat <$> many (quote <|> escQuote <|> slash <|> escSlash <|> escBackslash <|> newLine <|> maybeEscaped)
  where quote = do
          char '"'
          inQuote <- getState
          putState $ not inQuote
          return "\""
        escQuote = try $ string "\\\""
        slash = char '/' >> return "/"
        escSlash = try $ string "\\/"
        escBackslash = try $ string "\\\\"
        newLine = do
          eol <- endOfLine
          inQuote <- getState
          putState False
          return $ if inQuote then "\"" ++ eol else eol
        maybeEscaped = do
          inQuote <- getState
          maybeEscape <- optionMaybe $ char '\\'
          symbol <- anyChar
          case (inQuote, maybeEscape) of
            (False, Just _) -> return $ '\\' : [symbol]
            (False, Nothing) -> return [symbol]
            (True, Just _) -> return [symbol]
            (True, Nothing) -> return $ '\\' : [symbol]

-- Skip this token, or be at the end of a line
skipOrEnd :: Parsec String () a -> Parsec String () ()
skipOrEnd p = (p >> return ()) <|> (lookAhead endOfLine >> return ()) <|> eof

-- Special expressions
flat :: Expr
flat = Sized (0, Nothing) (0, Just 0) AnyRect
thin :: Expr
thin = Sized (0, Just 0) (0, Nothing) AnyRect

-- Make a finite character class from string
mkSomeChar :: Bool -> [Maybe Char] -> Expr
mkSomeChar isPos = SomeChar isPos . fromAscList . sort

-- Shorthand for nonterminal in standard orientation
stdVar :: Label -> Expr
stdVar = Var (Rot 0)

-- Single-character expressions
reservedChars :: [(Char, Expr)]
reservedChars = [('$', AnyRect),
                 ('.', AnyChar),
                 ('f', flat),
                 ('t', thin),
                 ('_', stdVar Nothing),
                 ('b', Border),
                 ('e', Edge),
                 ('d', mkSomeChar True $ map Just ['0'..'9']),
                 ('u', mkSomeChar True $ map Just ['A'..'Z']),
                 ('l', mkSomeChar True $ map Just ['a'..'z']),
                 ('a', mkSomeChar True $ map Just $ ['A'..'Z'] ++ ['a'..'z']),
                 ('n', mkSomeChar True $ map Just $ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']),
                 ('s', mkSomeChar True $ map Just $ ['!'..'/'] ++ [':'..'@'] ++ ['['..'`'] ++ ['{'..'~'])]

-- Parse an escaped literal
escapedLit :: Parsec String () Expr
escapedLit = do
  char '\\'
  c <- anyChar
  return $ mkSomeChar True $ [Just c]

-- Parse a nonterminal
nonterm :: Parsec String () Expr
nonterm = do
  label <- upper
  return $ stdVar $ Just label

-- Parse a reserved character
reserved :: Parsec String () Expr
reserved = do
  char <- oneOf $ fst <$> reservedChars
  let Just expr = lookup char reservedChars
  return expr

-- Parse a character class
charClass :: Parsec String () Expr
charClass = char '[' `between` (skipOrEnd $ char ']') $ do
  include <- many $ try classRange <|> try border <|> pure <$> Just <$> classChar
  maybeExclude <- optionMaybe $ char ',' >> (many $ try classRange <|> try border <|> pure <$> Just <$> classChar)
  return $ case (null include, maybeExclude) of
    (True, Nothing) -> mkSomeChar False []
    (False, Nothing) -> mkSomeChar True $ concat include
    (True, Just exclude) -> mkSomeChar False $ concat exclude
    (False, Just exclude) -> mkSomeChar True $ concat include \\ concat exclude
  where needsEscape = "[]-,\\"
        classChar = noneOf (needsEscape ++ "\n\r") <|> (char '\\' >> oneOf needsEscape)
        border = string "\\b" >> return [Nothing]
        classRange = do
          a <- classChar
          char '-'
          b <- classChar
          return $ map Just [a..b]

-- Parse a numeric range with default minimum
numRange :: Int -> Parsec String () Range
numRange m = do
  lower <- many digit
  maybeDash <- optionMaybe $ char '-'
  upper <- many digit
  let lowerNum = if null lower then m else read lower
      upperNum = if null upper then Nothing else Just $ read upper
  return $ case maybeDash of
    Nothing -> (lowerNum, if null lower then Nothing else Just lowerNum)
    Just _ -> (lowerNum, upperNum)

-- Parse two numeric ranges with default minima
numRange2D :: Int -> Parsec String () (Range, Range)
numRange2D m = do
  xRange <- numRange 0
  maybeYRange <- optionMaybe $ char ',' >> numRange 0
  return $ case maybeYRange of
    Nothing -> (xRange, xRange)
    Just yRange -> (xRange, yRange)

-- Parse a size constraint
sizeConstr :: Parsec String () (Expr -> Expr)
sizeConstr = do
  char '{'
  (xRange, yRange) <- numRange2D 0
  optionMaybe $ char '}'
  return $ Sized xRange yRange

-- Encoding of orientations
charToD4 :: Char -> [D4]
charToD4 '0' = [Rot 0]
charToD4 '1' = [Rot 1]
charToD4 '2' = [Rot 2]
charToD4 '3' = [Rot 3]
charToD4 '4' = [RefRot 0]
charToD4 '5' = [RefRot 1]
charToD4 '6' = [RefRot 2]
charToD4 '7' = [RefRot 3]
charToD4 'O' = [Rot 0, Rot 1, Rot 2, Rot 3, RefRot 0, RefRot 1, RefRot 2, RefRot 3]
charToD4 'X' = [Rot 0, Rot 1, Rot 2, Rot 3]
charToD4 'N' = [Rot 0, Rot 2]
charToD4 'T' = [Rot 0, RefRot 0]
charToD4 'K' = [Rot 0, RefRot 2]
charToD4 'H' = [Rot 0, RefRot 0, RefRot 2, Rot 2]
charToD4 'A' = [Rot 0, RefRot 1]
charToD4 'D' = [Rot 0, RefRot 3]
charToD4 'C' = [Rot 0, RefRot 1, RefRot 3, Rot 2]
charToD4 'F' = []

-- Parse a set of orientations
orientationSet :: Parsec String () (Expr -> Expr)
orientationSet = do
  char 'o'
  choices <- many1 $ oneOf "01234567OXNTKHADCF"
  optionMaybe $ char '}'
  let transformations = \expr ->
        [Fixed expr | 'F' `elem` choices] ++ nub [orient expr rot | ch <- choices, rot <- charToD4 ch]
  return $ foldr1 (:|) . transformations

-- Parse a postfix grid specification
gridSpec :: Parsec String () (Expr -> Expr)
gridSpec = do
  char ':'
  (xRange, yRange) <- numRange2D 1
  optionMaybe $ char '}'
  return $ Grid xRange yRange

-- Parse a counting specification
countSpec :: Parsec String () (Expr -> Expr)
countSpec = do
  char '#'
  range <- numRange 1
  optionMaybe $ char '}'
  return $ Count range
  
-- Parse a context anchor
anchor :: Parsec String () Expr
anchor = do
  label <- oneOf ['0'..'9']
  return $ Anchor $ read [label]

-- Parse an expression
expression :: Parsec String () Expr
expression = mkPrattParser opTable term <?> "expression"
  where term = parenthesized <|> quoted <|> inContext <|> anchor <|> nonterm <|> reserved <|> escapedLit <|> charClass <?> "term"
        parenthesized = char '(' `between` (skipOrEnd $ char ')') $ expression
        quoted = char '"' `between` char '"' $ expression
        inContext = char '<' `between` (skipOrEnd $ char '>') $ InContext <$> expression
        basicInfixes = [lookAhead term >> return (:>),
                        char '/' >> lookAhead term >> return (:^)] ++
                       map addPostfix
                       [char ' ' >> return (:>),
                        char '&' >> return (:&),
                        char '-' >> return (\e1 e2 -> e1 :& Not e2),
                        char '|' >> return (:|),
                        char '~' >> return (:~)]
        opTable = [[Postfix $ try $ char '^' >> postfix]] ++
                  map (return . InfixR . try . (char '^' >>)) basicInfixes ++
                  [[Postfix $ try $ postfix]] ++
                  map (return . InfixR . try) basicInfixes ++
                  [[Postfix $ try $ char 'v' >> postfix]] ++
                  map (return . InfixR . try . (char 'v' >>)) basicInfixes
        postfixes = [sizeConstr,
                     orientationSet,
                     gridSpec,
                     countSpec,
                     char '\'' >> return Fixed,
                     char '?' >> return (thin :|),
                     try (string "/?") >> return (flat :|),
                     char '+' >> return (Grid (1, Nothing) (1, Just 1)),
                     try (string "/+") >> return (Grid (1, Just 1) (1, Nothing)),
                     char '*' >> return (Grid (0, Nothing) (1, Just 1)),
                     try (string "/*") >> return (Grid (1, Just 1) (0, Nothing)),
                     char '!' >> return Not]
        postfix = do
          operations <- many1 $ choice postfixes
          return $ foldr1 (flip (.)) operations
        addPostfix :: Parsec String () (Expr -> Expr -> Expr) -> Parsec String () (Expr -> Expr -> Expr)
        addPostfix parseOp = do
          op <- parseOp
          pfix <- optionMaybe postfix
          return $ case pfix of
            Just p -> \e1 e2 -> p $ op e1 e2
            Nothing -> op

-- Parse a line of a grammar file into options, label, and expression
contentLine :: Parsec String () ([Option], (Label, Expr))
contentLine = do
  opts <- try (options <* char '`') <|> return []
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
grammarLine :: Parsec String () (Maybe ([Option], (Label, Expr)))
grammarLine = try (commentLine >> return Nothing) <|> (Just <$> contentLine)

-- File parser
parseGrFile :: String -> String -> Either ParseError ([Option], Map Label Expr)
parseGrFile filename grammar = foldToMap <$> contents
  where contents :: Either ParseError [([Option], (Label, Expr))]
        contents = do
          unquoted <- runParser quoted False filename grammar
          catMaybes <$> parse (grammarLine `sepEndBy` endOfLine <* eof) ("pre-processed " ++ filename) unquoted
        foldToMap triples = (firsts, folded)
          where firsts = concat $ fst <$> triples
                folded = foldr (\(label, expr) assoc -> insert label expr assoc) empty $ snd <$> triples

-- Parse a matrix from newline-delimited string
parseMatFile :: String -> (Size, Map Coord Char)
parseMatFile s = ((w, h), fromList pairs)
  where rows = lines s
        pairs = [((x, y), c) | (y, row) <- zip [0..] rows, (x, c) <- zip [0..] row]
        w = maximum $ 0 : map length rows
        h = length rows

-- Parse a chain of options form a string
parseOptions :: String -> [Option]
parseOptions str = case parse options "" str of
  Right result -> result
  Left _ -> []
