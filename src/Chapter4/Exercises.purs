module ExercisesChapter4
  (
    main
  ) where

import Data.Ring
import Data.Function
import Data.Show
import Data.Maybe
import Data.Functor
import Data.Ord
import Data.Eq
import Data.HeytingAlgebra
import Data.EuclideanRing
import Data.Int
import Data.List.Lazy
import Math
import Control.MonadZero
import Partial.Unsafe
import Control.Bind as Bind
import Control.Monad.Eff.Console as Console
import Data.Array as Array
import Data.Either.Nested (in1)
import Node.Buffer (BufferValueType(..))
import Prelude (class BooleanAlgebra)


main = do
  Console.log $ show $ pythagoreanTriple (2 * 3 * 5 * 7 * 2)

isEvenInteger :: Int -> Boolean
isEvenInteger x =
  case x of
    0 -> true
    1 -> false
    y -> isEvenInteger (y - 2)

isEvenIntegerR :: Array Int -> Int -> Int
isEvenIntegerR array accum = 
  case maybeTail of 
    Nothing -> accum
    Just (x) ->
      let 
        headAsNumber = unsafePartial(fromJust (maybeHeadA))
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
  i <- Array.range 1 n
  j <- Array.range 1 n
  k <- Array.range 1 n
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

firstFactor :: Int -> Maybe Int
firstFactor n = if n == 1 then Nothing else Just $ firstFactor2 n 2

firstFactor2 :: Int -> Int -> Int
firstFactor2 n a = 
  if a == n then
    n 
  else
    if (toNumber n) % (toNumber a) == 0.0 then 
      a
    else 
      firstFactor2 n (a + 1)

factors2 :: Int -> Array Int -> Array Int
factors2 n arr =
  let 
    firstFactorOfNMaybe = firstFactor n
  in case firstFactorOfNMaybe of
    Nothing -> 
      arr
    Just x ->
      factors2 (n/x) $ Array.cons x arr 

factors :: Int -> Array Int
factors n = factors2 n []

areAllValuesTrue :: Array Boolean -> Boolean
areAllValuesTrue = foldl (&&) true

count_ :: forall a. (a -> Boolean) -> Array a -> Int
count_ f = foldl (\ accum x -> if f(x) then accum + 1 else accum) 0