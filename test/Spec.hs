import Interpolation (Point, linearSpline, newton)
import StreamProcessor (Config (..), processStream)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [mathTests, streamTests]

mathTests :: TestTree
mathTests =
  testGroup
    "Math"
    [ testCase "Linear Interpolation: Middle" $
        linearSpline [(0, 0), (2, 2)] 1.0 @?= 1.0,
      testCase "Linear Spline: Selects correct segment" $ do
        let points = [(0, 0), (1, 1), (2, 0)]
        linearSpline points 0.5 @?= 0.5
        linearSpline points 1.5 @?= 0.5,
      testCase "Newton Interpolation: y = x^2" $
        let points = [(1, 1), (2, 4), (3, 9)]
         in newton points 2.5 @?= 6.25
    ]

streamTests :: TestTree
streamTests =
  testGroup
    "StreamProcessor"
    [ testCase "Linear Stream (Window 2)" $ do
        let cfg = Config {stepSize = 0.5, windowSize = 2, method = "linear"}
        let input = [(0, 0), (1, 1), (2, 2)] :: [Point]
        let result = processStream cfg input
        let actualY = map (snd . snd) result
        let expectedY = [0.0, 0.5, 1.0, 1.5, 2.0]

        expectedY @?= actualY,
      testCase "Stream Newton (Window 4, Parabola)" $ do
        let cfg = Config {stepSize = 0.5, windowSize = 4, method = "newton"}
        let input = [(0, 0), (1, 1), (2, 4), (3, 9), (4, 16)] :: [Point]
        let result = processStream cfg input
        let actualY = map (snd . snd) result
        let expectedY = [0.0, 0.25, 1.0, 2.25, 4.0, 6.25, 9.0, 12.25, 16.0]

        expectedY @?= actualY
    ]
