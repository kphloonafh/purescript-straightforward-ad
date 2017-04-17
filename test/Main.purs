module Test.Main where

import AD
import Data.Array
import Control.Monad.Eff.Console as C
import Control.Monad.Eff
import Control.Apply
import Data.Traversable
import Prelude

numericDiff f x = (f (x + h) - f x) / h where h = 1e-12

main = 
  let fs :: ∀ a. Differentiable a => Array (a -> a)
      fs = [sin,cos,tan,exp,log,asin,acos,atan,sqr,sqrt,recip]
      xs = zipWith div (flip numericDiff a1 <$> fs) (flip diff a1 <$> fs)
      a1 = 8.8

      fv [a,b,c] = Lift 2.3 + a + b + c
      fv _ = zero
      vec = [1999.2, 2323.2, 3.3]

      f1 :: ∀ a. Differentiable a => a -> a
      f1 x = sin x * x
      a = 988.0

      {--for some reason multiplication gives result bigger by one--}
   in do
     sequence_ $ map C.logShow xs
     {--C.logShow $ grad fv vec--}

     C.log "------"
     C.logShow $ numericDiff f1 a
     C.logShow $ diff f1 a
     pure unit
