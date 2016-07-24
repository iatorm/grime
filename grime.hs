-- A two-dimensional language based on Boolean grammars

import Expression
import Matcher (matchAllEmpty)
import Parser (parseMatFile, parseGrFile)
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
        ['-':a, b, c] -> (a, b, c)
        [a, b] -> ("", a, b)
        _ -> error "Incorrect arguments. Usage: grime [opts] grammarfile patternfile"
  parsedGrammar <- fmap parseGrFile $ readFile grFile
  case parsedGrammar of
    Left parseError -> print parseError
    Right (fileOpts, grammar) -> do
      pict <- readFile matFile
      let opts = [o | o <- nub $ cmdOpts ++ fileOpts, elem o cmdOpts /= elem o fileOpts]
          (sze@(wMat, hMat), mat) = parseMatFile pict
          (minX, minY, numX, numY) = if elem 'b' opts then (-1, -1, wMat+1, hMat+1) else (0, 0, wMat, hMat)
          (matches, logs) = matchAllEmpty (Context sze mat grammar) $
                            if elem 'e' opts
                            then [(minX, minY, numX, numY)]
                            else [(x, y, w, h) |
                                  w <- [0..numX], h <- [0..numY],
                                  x <- [minX..numX-w], y <- [minY..numY-h]]
          finalMatches = if elem 'a' opts || elem 'n' opts then matches else take 1 matches
      when (elem 'd' opts) $ do
        putStrLn $ "Options: " ++ opts
        putStrLn $ "Input dimensions: " ++ show sze
        putStrLn "Definitions:"
        forM_ (toList grammar) $ \(l, e) ->
          putStrLn $ " " ++ (case l of Nothing -> "Toplevel pattern = "; Just a -> a:" = ") ++ show e
        putStr logs
      if (elem 'n' opts /= elem 'e' opts)
        then print $ length finalMatches
        else forM_ finalMatches $ \rect -> do
        when (elem 'p' opts) $ print rect
        when (not $ elem 's' opts) . putStrLn $ submatrix rect pict
