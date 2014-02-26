module LambdaRay.Film where
import LambdaRay.Helper
import LambdaRay.Types
import Control.Monad (forM_)
import Control.Monad.ST.Safe
import qualified Codec.Picture as CI
import qualified Data.Array.ST as A
import Data.Array

type ArrayT s = (A.STArray s (Int, Int) (Int, Spectrum))
avg :: Int -> Int -> [Sample] -> Array (Int,Int) (Int, Spectrum)
avg w h samples = A.runSTArray (do
      array <- A.newArray ((0,0), (w,h)) (0, nullSpectrum) :: ST s (ArrayT s)
      forM_ samples (update array)
      return array)
  where
    update array (Sample x y v) = do
      let x'=floor x
      let y'=floor y
      (count, spectrum) <- A.readArray array (x', y')
      let count' = count + 1 :: Int
      let frac = (fromIntegral count :: Float) / fromIntegral count'
      let spectrum' = ((frac `scaleSpectrum` spectrum) + v)
      A.writeArray array (x',y') (count', spectrum')

boxFilterFilm :: Film
boxFilterFilm scene samples = CI.generateImage (develop samples) w h
  where
    w = fromInteger $ width scene :: Int
    h = fromInteger $ height scene :: Int
    develop :: [Sample] -> Int -> Int -> CI.PixelRGBF
    develop samples x y = snd $ avg w h samples ! (x,y)
