module LambdaRay.Film where
import LambdaRay.Helper
import Data.Map

binning samples = fromListWith (++) [((toInteger x, toInteger y), [v]) | Sample x y v]

boxFilterFilm :: Film
boxFilterFilm scene samples = CI.generateImage (develop samples) (fromInteger $ width scene) (fromInteger $ height scene)
  where
    develop :: (CI.Pixel a) =>  [Sample] -> Int -> Int -> a
    develop samples x y = average $ (binning samples) !! (toInteger x, toInteger y)
