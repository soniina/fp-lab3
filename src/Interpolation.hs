module Interpolation (Point, linear, newton) where

type Point = (Double, Double)

linear :: Point -> Point -> Double -> Double
linear (x0, y0) (x1, y1) x = y0 + (x - x0) * (y1 - y0) / (x1 - x0)

newton :: [Point] -> Double -> Double
newton points x =
  let coeffs = [dividedDifference (take k points) | k <- [1 .. length points]]

      xs = map fst points
      terms = scanl (*) 1.0 [x - xi | xi <- init xs]
   in sum $ zipWith (*) coeffs terms

dividedDifference :: [Point] -> Double
dividedDifference [(_, y)] = y
dividedDifference points =
  let left = init points
      right = tail points
      (xFirst, _) = head points
      (xLast, _) = last points
   in (dividedDifference right - dividedDifference left) / (xLast - xFirst)
