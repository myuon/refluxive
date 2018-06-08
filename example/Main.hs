{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Main where

import qualified SDL as SDL
import Control.Monad
import Data.Extensible
import Linear.V2
import Linear.V4
import Lefrect

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Application" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  loop renderer

  where
    loop renderer = do
      events <- SDL.pollEvents
      keyQuit <- return $ flip any events $ \ev -> case SDL.eventPayload ev of
        SDL.KeyboardEvent (SDL.KeyboardEventData _ _ _ (SDL.Keysym SDL.ScancodeQ _ _)) -> True
        _ -> False

      SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 255 255
      SDL.clear renderer

      render renderer (GridLayout 50 50) $ translate (V2 100 100) $ graphics $
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

      SDL.present renderer
      unless keyQuit $ loop renderer

