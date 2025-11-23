module StreamProcessor (Config, ResultPoint, processStream) where

import Interpolation (Point, linearSpline, newton)

data Config = Config
  { stepSize :: Double,
    windowSize :: Int,
    method :: String
  }
  deriving (Show)

type ResultPoint = (String, Point)

generatePoints :: Double -> Double -> Double -> [Double]
generatePoints start end step =
  takeWhile (< end) [start, start + step ..]

processStream :: Config -> [Point] -> [ResultPoint]
processStream config = accumulate []
  where
    mid = windowSize config `div` 2

    accumulate :: [Point] -> [Point] -> [ResultPoint]
    accumulate _ [] = []
    accumulate buffer (p : ps)
      | length buffer < windowSize config - 1 = accumulate (buffer ++ [p]) ps
      | otherwise = stepStart config (buffer ++ [p]) ps

    stepStart :: Config -> [Point] -> [Point] -> [ResultPoint]
    stepStart cfg window restInput =
      let pStart = head window
          pMid = window !! mid

          xs = generatePoints (fst pStart) (fst pMid) (stepSize cfg)

          startPoints = calculatePoints cfg window xs
       in startPoints ++ stepLoop cfg window restInput

    stepLoop :: Config -> [Point] -> [Point] -> [ResultPoint]
    stepLoop cfg buffer [] = stepFinal cfg buffer
    stepLoop cfg buffer (nextP : rest) =
      let newBuffer = tail buffer ++ [nextP]
          pPreMid = newBuffer !! (mid - 1)
          pMid = newBuffer !! mid

          xs = generatePoints (fst pPreMid) (fst pMid) (stepSize cfg)
          loopPoints = calculatePoints cfg newBuffer xs
       in loopPoints ++ stepLoop cfg newBuffer rest

    stepFinal :: Config -> [Point] -> [ResultPoint]
    stepFinal cfg buffer =
      let pMid = buffer !! mid
          pEnd = last buffer

          xs = generatePoints (fst pMid) (fst pEnd + stepSize cfg / 2) (stepSize cfg)
       in calculatePoints cfg buffer xs

calculatePoints :: Config -> [Point] -> [Double] -> [ResultPoint]
calculatePoints cfg buffer = concatMap calcForX
  where
    calcForX :: Double -> [ResultPoint]
    calcForX x = case method cfg of
      "linear" ->
        [("linear", (x, linearSpline buffer x))]
      "newton" ->
        [("newton", (x, newton buffer x))]
      "both" ->
        [ ("linear", (x, linearSpline buffer x)),
          ("newton", (x, newton buffer x))
        ]
      _ -> []
