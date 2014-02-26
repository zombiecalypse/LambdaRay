{-# LANGUAGE ForeignFunctionInterface #-}
module LambdaRay.Film where
import LambdaRay.Helper
import LambdaRay.Types
import Control.Monad (forM_)
import Control.Monad.ST.Safe
import qualified Codec.Picture as CI
import qualified Data.Array.ST as A
import Data.Array.Unboxed

import qualified Codec.Picture as CI

foreign import ccall unsafe "math.h floor"
    c_floor :: Double -> Int

tr = c_floor

avg :: Int -> Int -> [Sample] -> UArray (Int,Int, Int) Float
avg w h samples = A.runSTUArray (do
      array <- A.newArray ((0,0,0), (w,h,2)) 0:: ST s (A.STUArray s (Int, Int, Int) Float)
      counts <- A.newArray ((0,0,0), (w,h,2)) 0:: ST s (A.STUArray s (Int, Int, Int) Int)
      forM_ samples' (update array counts)
      return array)
  where
    samples' = pmap (\(Sample x y (CI.PixelRGBF r g b)) -> (tr x, tr y, r, g, b)) samples
    update array counts (x', y', r, g, b) = do
      let smpl = [r, g, b]
      forM_ [0,1,2] (\c -> do
        count <- A.readArray counts (x', y', c)
        chan <- A.readArray array (x', y', c)
        let count' = count + 1 :: Int
        let frac = (fromIntegral count :: Float) / fromIntegral count'
        let chan' = frac * chan + (1-frac) * (smpl !! c)
        A.writeArray counts (x',y', c) count'
        A.writeArray array (x',y', c) chan')

boxFilterFilm :: Film
boxFilterFilm scene samples = CI.generateImage (develop samples) w h
  where
    w = fromInteger $ width scene :: Int
    h = fromInteger $ height scene :: Int
    develop :: [Sample] -> Int -> Int -> CI.PixelRGBF
    develop samples x y = spec x y
    averg = avg w h samples
    spec x y = CI.PixelRGBF (averg ! (x,y,0)) (averg ! (x,y,1)) (averg ! (x,y,2))
