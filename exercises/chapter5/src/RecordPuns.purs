module RecordPuns where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Picture (Point(..), Shape(..))


originCirc :: Shape
originCirc = Circle (Point { x, y }) 10.0
  where x = 0.0
        y = 0.0

doubleAndCenter :: Shape -> Shape
doubleAndCenter (Circle c r)      = Circle (Point { x: 0.0, y: 0.0 }) (2.0 * r)
doubleAndCenter (Rectangle c w h) = Rectangle 
                                      (Point { x: -w, y: -h }) 
                                      (2.0 * w) (2.0 * h)
doubleAndCenter (Line 
                  (Point { x: x0, y: y0 }) 
                  (Point { x: x1, y: y1 }))  
                                  = Line 
                                      (Point { x: x0 - x1, y: y0 - y1 }) 
                                      (Point { x: -x0 + x1, y: -y0 + y1 }) 
doubleAndCenter (Text p text)     = Text (Point { x: 0.0, y: 0.0 }) text

extractText :: Shape -> Maybe String
extractText (Text _ text) = Just text
extractText _             = Nothing