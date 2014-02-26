module LambdaRay.Sampler where
import Numeric.LinearAlgebra
import LambdaRay.Types

randomSampler seed n d = take n $ [toList $ randomVector i Uniform d | i <- iterate (+1) seed]

oneSampler n d = take n $ repeat $ take d $ repeat 0.5
