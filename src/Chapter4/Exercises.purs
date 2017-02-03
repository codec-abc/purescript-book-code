module ExercisesChapter4
  (
    main
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromJust)
import Data.Int (toNumber)
import Math ((%))
import Control.MonadZero (guard)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff.Console as Console
import Data.Array as Array
import Data.List.Lazy (repeat, take)
import Control.Monad.Eff (Eff)


main :: forall t. Eff ( "console" :: Console.CONSOLE | t) Unit
main = do
  Console.log $ show $ factors $ 2 * 3 * 4 * 5

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

infixl 4 Array.filter as <$?>

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

decomposeInPrimeFactors_ :: Int -> Array Int -> Array Int
decomposeInPrimeFactors_ n arr =
  let 
    firstFactorOfNMaybe = firstFactor n
  in case firstFactorOfNMaybe of
    Nothing -> 
      arr
    Just x ->
      decomposeInPrimeFactors_ (n/x) $ Array.cons x arr 

decomposeInPrimeFactors :: Int -> Array Int
decomposeInPrimeFactors n = decomposeInPrimeFactors_ n []

areAllValuesTrue :: Array Boolean -> Boolean
areAllValuesTrue = Array.foldl (&&) true

count_ :: forall a. (a -> Boolean) -> Array a -> Int
count_ f = Array.foldl (\ accum x -> if f(x) then accum + 1 else accum) 0

factorsPermutations :: Array Int -> Array (Array Int)
factorsPermutations a = let length_ = Array.length a in
  case length_ of
    0 -> [a]
    1 -> [a]
    2 -> [a]
    _ -> 
      let 
        firstElem = unsafePartial fromJust $ Array.head a
        tail_ = unsafePartial fromJust $ Array.tail a
        permutationsForThisLevel = factorsPermutations_ tail_ firstElem
        permutationsForLowerLevels = do 
          w <- permutationsForThisLevel
          factorsPermutations w
      in
        Array.union (Array.cons a permutationsForThisLevel) permutationsForLowerLevels

factorsPermutations_ :: Array Int -> Int -> Array (Array Int)
factorsPermutations_ a b =
  let
    arrayLength = Array.length a
    repeatedArray = Array.fromFoldable (take arrayLength $ repeat a)
    y = Array.fromFoldable (take arrayLength $ repeat 1)
    --z = Array.range 0 (arrayLength - 1) >>= 
    --  \x -> [unsafePartial fromJust $ Array.updateAt x b y]
    z = do
      w <- Array.range 0 (arrayLength - 1)
      [unsafePartial fromJust $ Array.updateAt w b y]
  in Array.zipWith f repeatedArray z
    where f = Array.zipWith (\i j -> i * j)

factors :: Int -> Array (Array Int)
factors a = 
  let 
    allPermutations = factorsPermutations $ decomposeInPrimeFactors a
    sortedPerm = map Array.sort allPermutations
  in 
    Array.nubBy (==) sortedPerm

