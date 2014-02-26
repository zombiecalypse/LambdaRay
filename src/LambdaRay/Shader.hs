module LambdaRay.Shader where
import LambdaRay.Types
import qualified Codec.Picture as CI

shade :: Integrator -> Maybe HitRecord -> Spectrum
shade _ Nothing = CI.PixelRGBF 0 0 0
