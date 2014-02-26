module LambdaRay.Scene where
import LambdaRay.Camera
import LambdaRay.Shader
import LambdaRay.Types

emptyScene = Scene {
  outputPath = "test.png",
  samplesPerPixel = 2,
  width = 600,
  height = 480,
  camera = dummyCameraCast,
  integrator = undefined,
  root = const Nothing,
  lights = []
}
