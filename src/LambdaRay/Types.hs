module LambdaRay.Types where
import Numeric.LinearAlgebra
import qualified Codec.Picture as CI

data CameraCast = CameraCast { castRay :: Integer -> Integer -> Integer -> Integer -> [Float] -> Ray }

data Ray = Ray { origin :: Vector Float, direction :: Vector Float }
data Sampler = Sampler { runSampler :: Integer -> Integer -> [Vector Float] }
data Spectrum = Rgb Float Float Float
data Integrator = Integrator { runIntegrator :: Ray -> Spectrum }
data Sample = Sample Float Float Spectrum
data Intersectable = Intersectable { runIntersectable :: Ray -> Maybe HitRecord }
data Material = Material
data Light = PointLight (Vector Float)
data HitRecord = HitRecord {
  position     :: Vector Float,
  t            :: Float,
  hit          :: Intersectable,
  oppIncident  :: Vector Float,
  material     :: Material,
  normal       :: Vector Float,
  tangents     :: (Vector Float, Vector Float),
  texCoords    :: Maybe (Float, Float)
}

data Scene = Scene { 
  outputPath      :: String,
  samplesPerPixel :: Integer,
  width           :: Integer,
  height          :: Integer,
  camera          :: CameraCast,
  integrator      :: Integrator,
  root            :: Intersectable,
  lights          :: [Light]
}

type Film = Scene -> [Sample] -> CI.Image Float
