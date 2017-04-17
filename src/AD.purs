module AD (module AD.Differentiable, module AD) where

import Data.Traversable
import Data.Maybe
import Prelude
import AD.Differentiable
import Control.Apply

data Diff a = Diff a a | Lift a

chain f f' (Diff a a') = Diff (f a) (a' * f' a)
chain f _  (Lift a)    = Lift (f a)

evalDiff (Diff u _) = u
evalDiff (Lift a) = a

extractDiff (Diff _ u') = u'
extractDiff (Lift _) = zero

diff :: ∀ a. Differentiable a => (Diff a -> Diff a) -> a -> a
diff  f = extractDiff <<< f <<< flip Diff one

diff2 :: ∀ a. Differentiable a => (Diff a -> Diff a) -> a -> a
diff2 f = extractDiff <<< f <<< f <<< flip Diff one

grad :: ∀ a t. Differentiable a => Traversable t
              => (t (Diff a) -> Diff a) -> t a -> t a
grad f t = map extractDiff $ _.value $ mapAccumL outer 1 t
  where outer i _   = { accum: i+1, value: f $ _.value $ mapAccumL (inner i) 1 t}
        inner i j a = { accum: j+1, value: if i == j then (Diff a one) else (Lift a) }

infix 0 chain as ><

two  = one + one
half = one / two
sec n = one / cos n
csc n = one / sin n

instance commutativediff :: CommutativeRing a => CommutativeRing (Diff a) 
instance diffEuclidean :: (Differentiable a, EuclideanRing a) => EuclideanRing (Diff a) where
  mod (Diff u u') (Diff v v') = Diff (u/v) ((u'* v - v'* u) / sqr v)
  mod (Diff u u') (Lift v)    = Diff (u/v)  (u'* v / sqr u)
  mod (Lift u)    (Diff v v') = Diff (u/v)  (v'* u / sqr u)
  mod (Lift u)    (Lift v)    = Lift (u/v)

  div (Diff u u') (Diff v v') = Diff (u/v) ((u'* v - v'* u) / sqr v)
  div (Diff u u') (Lift v)    = Diff (u/v)  (u'* v / sqr u)
  div (Lift u)    (Diff v v') = Diff (u/v)  (v'* u / sqr u)
  div (Lift u)    (Lift v)    = Lift (u/v)

  degree _ = 1

instance diffring :: Ring a => Ring (Diff a) where
  sub (Diff u u') (Diff v v') = Diff (u - v) (u' - v')
  sub (Lift u)    (Diff v v') = Diff (u - v) (-v')
  sub (Diff u u') (Lift v)    = Diff (u - v)  u'
  sub (Lift u)    (Lift v)    = Lift (u - v) 

instance semiDiffRing :: Semiring a => Semiring (Diff a) where
  zero = Lift zero
  one =  Lift one  

  add (Diff u u') (Diff v v') = Diff (u + v) (u'+ v')
  add (Lift u)    (Diff v v') = Diff (u + v)  v'
  add (Diff u u') (Lift v)    = Diff (u + v)  u'
  add (Lift u)    (Lift v)    = Lift (u + v)

  mul (Diff u u') (Diff v v') = Diff (u * v) (u'* v + v'* u)
  mul (Lift u)    (Diff v v') = Diff (u * v) (v'* u) 
  mul (Diff u u') (Lift v)    = Diff (u * v) (u'* v) 
  mul (Lift u)    (Lift v)    = Lift (u * v)

instance diffMathFunctions :: 
  (EuclideanRing a, Differentiable a) => Differentiable (Diff a) where
  sin  = sin    ><  cos
  cos  = cos    >< -sin
  tan  = tan    ><  \x -> sqr (sec x)
  exp  = exp    ><  exp
  sqr  = sqr    ><  mul two
  log  = log    ><  recip
  asin = asin   ><  \x ->  recip (sqrt (one - sqr x))
  acos = acos   ><  \x -> -recip (sqrt (one - sqr x))
  atan = atan   ><  \x ->  recip (one + sqr x)
  sqrt = sqrt   ><  \x ->  half / exp (half * log x)
  recip = recip ><  \x -> -recip (sqr x)
  pow x n = exp (n * log x)
