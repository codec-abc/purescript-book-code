module Excercises5
  (
    main
  ) where

import Control.Monad.Eff.Console as Console
import Data.Semiring
import Data.Ring

main = do
  Console.log "hello world"

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = (n - 1) * n