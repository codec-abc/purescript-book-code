module ExercisesChapter4
  (
    main
  ) where

import Prelude (class BooleanAlgebra)
import Data.Ring
import Data.Function
import Data.Show
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Functor
import Data.Ord
import Data.Either.Nested (in1)
import Data.Eq
import Data.HeytingAlgebra
import Control.Bind as Bind
import Control.MonadZero
import Control.Monad.Eff.Console as Console
import Partial.Unsafe
import Node.Buffer (BufferValueType(..))


main = do
  Console.log $ show $ pythagoreanTriple2 10

isEvenInteger :: Int -> Boolean
isEvenInteger x =
  case x of
    0 -> true
    1 -> false
    y -> isEvenInteger (y - 2)

isEvenIntegerR :: Array Int -> Int -> Int
isEvenIntegerR array accum = 
  case maybeTail of 
    Maybe.Nothing -> accum
    Maybe.Just (x) ->
      let 
        headAsNumber = unsafePartial(Maybe.fromJust (maybeHeadA))
        oneIfEvenElseZero = 
          if isEvenInteger headAsNumber 
            then 1 
            else 0
      in
        isEvenIntegerR x (accum + oneIfEvenElseZero) 
  where
    maybeTail = Array.tail array
    maybeHeadA = Array.head array

square :: Array Int -> Array Int
square = (<$>) (\x -> x * x)

filterOutNegative :: Array Int -> Array Int
filterOutNegative = (<$?>) (\x -> x >= 0)

filter_ = Array.filter

infixl 4 filter_ as <$?>

cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct a b = do
  x <- a
  y <- b
  pure [x,y]


isPyTriple :: forall a.                        
  ( Eq a
  , Semiring a
  , Ord a
  ) => a -> a -> a -> Boolean
isPyTriple i j k = (i * i + j * j == k * k) && i <= j

pythagoreanTriple :: Int -> Array (Array Int)
pythagoreanTriple n = do
  i <- 1 Array... n
  j <- 1 Array... n
  k <- 1 Array... n
  guard $ isPyTriple i j k
  [[i, j, k]]

pythagoreanTriple2 :: Int -> Array (Array Int)
pythagoreanTriple2 n =
  let result = 
        (Array.range 1 n)
        >>=
        \x ->
          (Array.range 1 n)
          >>=
          \y ->
            (Array.range 1 n)
            >>=
            \z ->
              if isPyTriple x y z then
                [[x,y,z]] :: Array (Array Int)
              else
                [] :: Array (Array Int)
  in 
    result