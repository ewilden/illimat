module LensTutorial where

import           Prelude
-- import           Control.Lens.Tutorial
import           Control.Lens            hiding ( element )
import           Control.Lens.TH

data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

$(makeLenses ''Atom)
$(makeLenses ''Point)

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)
