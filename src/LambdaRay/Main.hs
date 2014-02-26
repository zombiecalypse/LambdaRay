module LambdaRay.Main where
import LambdaRay.Helper
import LambdaRay.Types
import LambdaRay.Film
import LambdaRay.Camera
import LambdaRay.Sampler
import LambdaRay.Light
import LambdaRay.Shader

import Numeric.LinearAlgebra
import Data.Word (Word16)
import System.Random (randomIO)
import qualified Codec.Picture as CI
import qualified Codec.Picture.Types as CIT

evalSample :: Scene -> ((Int, Int), [Double]) -> [Sample]
evalSample scene ((x,y), (dx:dy:samples)) = spectralSample
  where
    spectralSample = [Sample (fromIntegral x+dx) (fromIntegral y+dy) (shade (integrator scene) $ root scene ray)]
    ray = camera scene x y w h samples
    w = fromInteger $ width scene :: Int
    h = fromInteger $ height scene :: Int

instance CIT.ColorConvertible CIT.PixelRGBF CIT.PixelRGB16 where
  promotePixel (CIT.PixelRGBF r g b) = CI.PixelRGB16 (c r) (c g) (c b)
    where
      c :: Float -> Word16
      c f = fromIntegral $ round (scale * f) :: Word16
      scale = fromIntegral (256*256)


mainFunc scene = do
    seed <- randomIO
    
    let sampler = randomSampler seed (w*h) (fromInteger $ 2 + samplesPerPixel scene)
    let samples = concat $ pmap (evalSample scene) (zip coordinates sampler)
    let image = boxFilterFilm scene samples
    let codedImage = CIT.promoteImage image :: CI.Image CI.PixelRGB16
    CI.writePng (outputPath scene) codedImage
  where
    coordinates = [(x, y) | x <- [0..w], y <- [0..h]]
    w = fromInteger $ width scene :: Int
    h = fromInteger $ height scene :: Int
