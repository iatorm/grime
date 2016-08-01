-- A two-dimensional language based on Boolean grammars

import Expression
import Matcher (matchAllEmpty)
import Parser (Option(..), parseMatFile, parseGrFile, parseOptions)
import Data.List (nub)
import Data.Map.Strict (toList)
import Control.Monad (forM_, when)
import System.Environment (getArgs)


-- Take a submatrix of a newline-delimited string
submatrix :: Rect -> String -> String
submatrix (x, y, w, h) = unlines . take h . drop y . map (take w . drop x) . lines

main :: IO ()
main = do
  args <- getArgs
  let (cmdOpts, grFile, matFile) = case args of
        ['-':a, b, c] -> (parseOptions a, b, c)
        [a, b] -> ([], a, b)
        _ -> error "Incorrect arguments. Usage: grime [opts] grammarfile patternfile"
  parsedGrammar <- fmap (parseGrFile grFile) $ readFile grFile
  case parsedGrammar of
    Left parseError -> print parseError
    Right (fileOpts, grammar) -> do
      pict <- readFile matFile
      let opts = [opt | opt <- nub $ cmdOpts ++ fileOpts, elem opt cmdOpts /= elem opt fileOpts]
          (sze@(wMat, hMat), mat) = parseMatFile pict
          (minX, minY, numX, numY) = if elem AddBorder opts then (-1, -1, wMat+1, hMat+1) else (0, 0, wMat, hMat)
          (matches, logs) = matchAllEmpty sze mat grammar $
                            if elem Entire opts
                            then [(minX, minY, numX, numY)]
                            else [(x, y, w, h) |
                                  w <- [numX,numX-1..0], h <- [numY,numY-1..0],
                                  x <- [minX..numX-w], y <- [minY..numY-h]]
          finalMatches = if elem AllMatches opts || elem Number opts then matches else take 1 matches
      when (elem Debug0 opts) $ do
        putStrLn $ "Options: " ++ show opts
        putStrLn $ "Input dimensions: " ++ show sze
        putStrLn "Definitions:"
        forM_ (toList grammar) $ \(l, e) ->
          putStrLn $ " " ++ (case l of Nothing -> "_ = "; Just a -> a:" = ") ++ show e
        when (elem Debug1 opts) $ putStr logs
      if (elem Number opts /= elem Entire opts)
        then print $ length finalMatches
        else forM_ finalMatches $ \rect -> do
        when (elem Positions opts) $ print rect
        when (not $ elem Patterns opts) . putStrLn $ submatrix rect pict
