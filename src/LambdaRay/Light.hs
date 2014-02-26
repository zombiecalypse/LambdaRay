module LambdaRay.Light where
import LambdaRay.Types

lightSample (PointLight v s) = LightSample v Nothing s 1
