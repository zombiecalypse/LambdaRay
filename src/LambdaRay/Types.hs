module LambdaRay.Types where
import Numeric.LinearAlgebra
import qualified Codec.Picture as CI

type Spectrum = CI.PixelRGBF
type Vect = Vector Double
type Mat = Matrix Double

data Ray = Ray { origin :: Vector Double, direction :: Vector Double }
data Sample = Sample Double Double Spectrum
data Material = Material
data Light = PointLight Vect Spectrum
-- point -> normal -> spectrum -> density
data LightSample = LightSample Vect (Maybe Vect) Spectrum Double
data HitRecord = HitRecord {
  position     :: Vector Double,
  t            :: Double,
  oppIncident  :: Vector Double,
  material     :: Material,
  normal       :: Vector Double,
  tangents     :: (Vector Double, Vector Double),
  texCoords    :: Maybe (Double, Double)
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

type Film = Scene -> [Sample] -> CI.Image CI.PixelRGBF
type Integrator = Ray -> Spectrum
type Sampler = Integer -> Integer -> [Vect]
type  CameraCast = Int -> Int -> Int -> Int -> [Double] -> Ray

type Intersectable = Ray -> Maybe HitRecord

instance Num CI.PixelRGBF where
  (CI.PixelRGBF r g b) + (CI.PixelRGBF r' g' b') = CI.PixelRGBF (r+r') (g+g') (b+b')
  (CI.PixelRGBF r g b) * (CI.PixelRGBF r' g' b') = CI.PixelRGBF (r*r') (g*g') (b*b')
  abs (CI.PixelRGBF r g b) = CI.PixelRGBF (abs r) (abs g) (abs b)
  signum (CI.PixelRGBF r g b) = CI.PixelRGBF (signum r) (signum g) (signum b)
  fromInteger i = CI.PixelRGBF (fromInteger i) (fromInteger i) (fromInteger i)

instance Fractional CI.PixelRGBF where
  (CI.PixelRGBF r g b) / (CI.PixelRGBF r' g' b') = CI.PixelRGBF (r/r') (g/g') (b/b')
  fromRational r = CI.PixelRGBF (fromRational r) (fromRational r) (fromRational r)
