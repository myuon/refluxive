{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications #-}
module Main where

import Control.Monad
import Data.Extensible
import Linear.V2
import Linear.V4
import Graphics.UI.Lefrect

main :: IO ()
main = runUI $ do
  counter <- setup @"counter"
  register (GridLayout 50 50) counter

  raw <- setup @"raw"
  register (GridLayout 50 50) $ rawGraphical raw $ translate (V2 100 100) $ graphics $
    [ colored (V4 30 30 30 255) $ graphics $
      [ rectangle (V2 0 0) (V2 1 1)
      , rectangle (V2 1 1) (V2 1 1)
      ]
    , colored (V4 255 255 255 255) $ graphics $
      [ rectangle (V2 1 0) (V2 1 1)
      , rectangle (V2 0 1) (V2 1 1)
      ]
    , colored (V4 200 100 100 255) $ rectangleWith (#fill @= False <: nil) (V2 4 4) (V2 1 1)
    , colored (V4 200 100 100 255) $ rectangleWith (#rounded @= Just 10 <: nil) (V2 5 5) (V2 1 1)
    ]

  mainloop

