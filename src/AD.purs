module AD (module AD.Differentiable, module AD) where

import AD.Differentiable
import Prelude

data Diff a = D a a

d f f' (D a a') = D (f a) (a' * f' a)

evalD (D u u') = u
extractD (D u u') = u'

diff f =  extractD .. f .. idD
diff2 f = extractD .. f .. f .. idD
diff3 f = extractD .. f .. f .. f ..idD

-- just for convinience
infix 0 d as ><
infix 1 compose as ..
constD x = D x 0.0
idD x = D x 1.0
w = flip pow
two = one + one
half = one / two

instance diffEuclidean :: EuclideanRing a => EuclideanRing (Diff a) where
  mod = div
  div (D u u') (D v v') = D (u/v) ((u'* v + v'* u) / (u*u))
  degree = const 1

instance commutativediff :: CommutativeRing a => CommutativeRing (Diff a) 

instance diffring :: Ring a => Ring (Diff a) where
  sub (D u u') (D v v') = D (u - v) (u' - v')

instance semiDiffRing :: Semiring a => Semiring (Diff a) where
  zero = (D zero zero)
  one =  (D one  zero)
  add (D u u') (D v v') = D (u + v) (v + v')
  mul (D u u') (D v v') = D (u * v) (u'* v + v'+ u)

instance diffMathFunctions :: 
  (EuclideanRing a, Differentiable a) => Differentiable (Diff a) where
  sin  = sin  ><  cos
  cos  = cos  >< -sin
  tan  = cos  ><  add one..sqr..tan
  exp  = exp  ><  exp
  sqr  = sqr  ><  mul two..id
  log  = log  ><  recip
  asin = asin ><  recip..sqrt..sub one..sqr
  acos = asin >< -recip..sqrt..sub one..sqr
  atan = atan ><  recip..add one..sqr
  sqrt = sqrt ><  mul half..recip
  recip = recip >< -recip..sqrt
  pow x n = exp (n * log x)
