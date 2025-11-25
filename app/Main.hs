module Main (main) where

import Interpolation (Point)
import StreamProcessor
import System.Environment (getArgs)
import Text.Printf (printf)

defaultConfig :: Config
defaultConfig =
  Config
    { stepSize = 1.0,
      windowSize = -1,
      method = "linear"
    }

parseArgs :: [String] -> Config -> Config
parseArgs [] cfg = cfg
parseArgs (arg : rest) cfg = fixWindowSize $ case arg of
  "--step" ->
    case rest of
      (val : r) -> parseArgs r (cfg {stepSize = read val})
      [] -> error "Error: --step requires a value"
  "--n" ->
    case rest of
      (val : r) -> parseArgs r (cfg {windowSize = read val})
      [] -> error "Error: --n requires a value"
  "--linear" -> parseArgs rest (cfg {method = "linear"})
  "--newton" -> parseArgs rest (cfg {method = "newton"})
  "--both" -> parseArgs rest (cfg {method = "both"})
  _ -> parseArgs rest cfg

fixWindowSize :: Config -> Config
fixWindowSize config@Config {method = m, windowSize = ws} =
  let minNeeded = if m == "linear" then 2 else 4
   in config {windowSize = max ws minNeeded}

parseLine :: String -> Point
parseLine line =
  let cleanLine = map (\c -> if c == ';' then ' ' else c) line
      wordsList = words cleanLine
   in case wordsList of
        (x : y : _) -> (read x, read y)
        _ -> error $ "Invalid input line format: " ++ line

formatResult :: ResultPoint -> String
formatResult (algoName, (x, y)) =
  printf "> %s: %g %g" algoName x y

main :: IO ()
main = do
  args <- getArgs

  let config = parseArgs args defaultConfig

  content <- getContents
  let inputLines = lines content
  let points = map parseLine (filter (not . null) inputLines)

  let results = processStream config points

  mapM_ (putStrLn . formatResult) results
