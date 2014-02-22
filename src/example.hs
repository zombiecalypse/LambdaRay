import Numeric.LinearAlgebra
import LambdaRay.Helper
import LambdaRay.Main

main = do
  let a = (3><3) ([1..] :: [Double])
  let b = (3><3) ([3..] :: [Double])
  print $ (a <> b)
