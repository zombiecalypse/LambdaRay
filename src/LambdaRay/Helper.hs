module LambdaRay.Helper where
import Numeric.LinearAlgebra
import qualified Control.Parallel.Strategies as P

pmap f as = P.withStrategy (P.parListChunk 1024 P.rpar) $ map f as

vec4 :: [Float] -> Vector Float
vec4 = (4 |>)
vec3 :: [Float] -> Vector Float
vec3 = (3 |>)


average :: Fractional a => [a] -> a
average l = sum [a/(fromIntegral $ length l) | a <- l]
