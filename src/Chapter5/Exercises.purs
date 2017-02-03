module ExercisesChapter5
  (
    main
  ) where

import Prelude
import Control.Monad.Eff.Console as Console
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)
import Math (pi)
import Control.Monad.Eff (Eff)

main :: forall t. Eff ( "console" :: Console.CONSOLE | t ) Unit
main = do
  Console.log "hello world"

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = (n - 1) * n

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity 
  { 
    address: 
      { 
        street : a, 
        city : b 
      } 
    } 
    { 
      address: 
        { 
          street : c, 
          city : d 
        }
    } = 
      a == c && b == d

fromSingleton :: forall a. a -> Array a -> a
fromSingleton defaultValue [x :: a] = x
fromSingleton defaultValue _ = defaultValue

data Shape = 
  Circle Point Number |
  Rectangle Point Number Number |
  Line Point Point |
  Text Point String

data Point = Point
  { 
    x :: Number,
    y :: Number
  }

originCircle :: Shape
originCircle =
  let origin = Point { x: 0.0, y: 0.0} in
  Circle origin 10.0

scaleShape :: Shape -> Number -> Shape
scaleShape shape scaleFactor =
  case shape of 
    Circle  pos a ->
     Circle pos (a * scaleFactor)
    Rectangle pos w h -> 
      Rectangle pos (w * scaleFactor) (h * scaleFactor)
    Line a b -> Line a b
    Text pos text -> Text pos text

textFromText :: Shape -> Maybe String
textFromText shape =
  case shape of 
    Text pos text -> Just text
    _ -> Nothing

type Picture = Array Shape

data Bounds = Bounds
  { 
    top    :: Number,
    left   :: Number,
    bottom :: Number,
    right  :: Number
  }

instance showPoint :: Show Point where
  show (Point p) =  "(" <> (show p.x) <> "," <> (show p.y) <> ")" 

instance showShape :: Show Shape where
  show shape = case shape of
    Circle pos radius -> 
      "Circle center : " <> (show pos) <> " with radius "  <> (show radius)
    Rectangle pos w h -> 
      "Rectangle center : " <> (show pos) <> " width "  <> (show w) <> " height " <> (show h)
    Line a b -> 
      "Line from : " <> (show a) <> " to " <> (show b)
    Text pos text -> 
      "Text at : " <> (show pos) <> " with content " <> text

emptyBounds :: Bounds
emptyBounds =
  Bounds 
      {
        top : 0.0,
        left : 0.0,
        bottom : 0.0,
        right : 0.0
      }

shapeBounds :: Shape -> Bounds
shapeBounds shape =
  case shape of 
    Circle (Point pos) radius ->
     Bounds
        {
          top : pos.y + radius,
          left : pos.x - radius,
          bottom : pos.y - radius,
          right : pos.x + radius
        }
    Rectangle (Point pos) w h -> 
      Bounds
        {
          top : pos.y + h,
          left : pos.x - w,
          bottom : pos.y - h,
          right : pos.x + w
        }
    Line (Point a) (Point b) ->
      Bounds
        {
           top : (max a.y b.y),
           left : (min a.x b.x),
           bottom : (min a.y b.y),
           right : (max a.x b.x)
        }
    Text (Point pos) text ->
      Bounds 
        {
          top : pos.y,
          left : pos.x,
          bottom : pos.y,
          right : pos.x
        }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
    combine :: Bounds -> Shape -> Bounds
    combine b shape = boundsUnion b $ shapeBounds shape
    boundsUnion :: Bounds -> Bounds -> Bounds
    boundsUnion d e = Bounds 
      {
        top : 0.0,
        left : 0.0,
        bottom : 0.0,
        right : 0.0
      }

aera :: Shape -> Number
aera shape =
  case shape of 
    Circle (Point pos) radius ->
      pi * radius * radius
    Rectangle (Point pos) w h -> 
      w * h
    Line (Point a) (Point b) ->
      0.0
    Text (Point pos) text ->
      0.0

newtype Complex = Complex
   { 
     real :: Number,
     imaginary :: Number
   }

instance showComplex :: Show Complex where
  show (Complex { real : real , imaginary : imaginary}) = 
    "(" <> (show real) <> ", " <> (show imaginary) <> "i)"