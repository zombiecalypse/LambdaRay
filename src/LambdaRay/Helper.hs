module LambdaRay.Helper where
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util(cross, norm)
import qualified Control.Parallel.Strategies as P
import qualified Codec.Picture as CI
import LambdaRay.Types

pmap f as = P.withStrategy (P.parListChunk 1024 P.rpar) $ map f as

vec4 :: [Double] -> Vect
vec4 = (4 |>)
vec3 :: [Double] -> Vect
vec3 = (3 |>)
mat3 :: [Double] -> Mat
mat3 = 3><3
mat4 :: [Double] -> Mat
mat4 = 4><4

zeroVec :: Int -> Vect
zeroVec = constant 0
zero3 = zeroVec 3
zero4 = zeroVec 4

vectFromF f = fromList $ map f [0..]
unitV i = vectFromF (\s -> if s == i then 1 else 0)

zeroMat x y = fromColumns $ take x $ repeat $ zeroVec y

normalize :: Vect -> Vect
normalize v = (1/norm v) `scale` v 

average :: Fractional a => [a] -> a
average l = sum [a/(fromIntegral $ length l) | a <- l]

rotationMatrix :: Vect -> Vect -> Mat
rotationMatrix up z = inv $ fromColumns [up' `cross` z', up', z', unitV 3]
  where
    up' = normalize up
    z' = normalize z

nullSpectrum = CI.PixelRGBF 0 0 0

scaleSpectrum :: Float -> CI.PixelRGBF -> CI.PixelRGBF
scaleSpectrum f (CI.PixelRGBF r g b) = CI.PixelRGBF (f*r) (f*g) (f*b)
