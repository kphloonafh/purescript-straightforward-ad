module AD.Differentiable (module AD.Differentiable) where

import Control.Apply
import Math as M
import Prelude

class (EuclideanRing n) <= Differentiable n where
  sin  :: n -> n
  cos  :: n -> n
  tan  :: n -> n
  exp  :: n -> n
  log  :: n -> n
  pow  :: n -> n -> n
  asin :: n -> n
  acos :: n -> n 
  atan :: n -> n
  sqr  :: n -> n 
  sqrt :: n -> n
  recip :: n -> n

instance differentiableNumber :: Differentiable Number where
  sin  = M.sin
  sqr x = x * x
  cos  = M.cos
  tan  = M.tan
  exp  = M.exp
  log  = M.log
  pow  = M.pow
  asin = M.asin
  acos = M.acos
  atan = M.atan
  sqrt = M.sqrt
  recip n = 1.0 / n
