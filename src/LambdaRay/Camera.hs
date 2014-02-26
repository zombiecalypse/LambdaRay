module LambdaRay.Camera(
  fixedCameraCast,
  dummyCameraCast,
  parameterizedCameraCast) where
import Numeric.LinearAlgebra
import LambdaRay.Helper
import LambdaRay.Types

parameterizedCameraCast :: Vect -> Vect -> Vect -> CameraCast
parameterizedCameraCast up lookat eye width height = matrixCameraCast m eye width height
  where
    m = (fixedCameraMatrix eye width height) <> rotation
    rotation = trans $ rotationMatrix up (lookat - eye)

matrixCameraCast :: Mat -> Vect -> CameraCast
matrixCameraCast m eye _ _ x y sample = Ray {
  origin = eye,
  direction = m <> v
} where
  v = (vec4 [x', y', -1, 1])
  x' = fromIntegral x + (sample !! 0) :: Double
  y' = fromIntegral y + (sample !! 1) :: Double

fixedCameraCast :: Vector Double -> CameraCast
fixedCameraCast eye width height = matrixCameraCast m eye width height where 
  m = fixedCameraMatrix eye width height

fixedCameraMatrix eye width height = center <> projection <> viewport
  where
    center = centerMatrix eye
    projection = inv $ fromBlocks [
      [ident 2, zeroMat 2 2],
      [zeroMat 2 2, proj]]
    proj = fromLists [
      [-(far+near)/(far-near), -(2*far*near)/(far - near)],
      [-1, 0]]
    far = 10.0
    near = 1.0
    width' = fromIntegral width
    height' = fromIntegral height
    viewport = inv $ mat4 [
      width'/2.0, 0,0,width'/2.0,
      0, height'/2.0, 0, height'/2.0,
      0, 0, 1, 0,
      0, 0, 0, 1]
  

dummyCameraCast :: CameraCast
dummyCameraCast width height x y sample = Ray {
  origin = vec3 [0..],
  direction = vec3 [fromIntegral x + sample !! 0, fromIntegral y + sample!!1, 1]
}

centerMatrix eye = fromColumns [zero4, zero4, zero4, eye]
