{-# LANGUAGE OverloadedStrings #-}
module HelloWorld where

import qualified SDL as SDL
import SDL.Vect
import Control.Monad
import Control.Monad.State
import Data.Extensible
import Data.Ix (inRange)
import Graphics.UI.Refluxive

instance Component UI "counter" where
  data Model "counter" = CounterModel Int
  data Signal "counter" = Clicked

  watcher _ =
    [ watch "builtin" $ \case
        BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (P pos)))) -> do
          when (inRange (V2 0 0, V2 200 100) pos) $ lift $ emit Clicked
          liftIO $ print "builtin watch!"
        _ -> return ()
    , watch "counter" $ \case
        Clicked -> do
          CounterModel c <- get
          modify $ \(CounterModel n) -> CounterModel (n+1)
          liftIO $ putStrLn $ "counter:" ++ show c
          return ()
    ]

  setup = new (CounterModel 0)

  getGraphical (CounterModel n) = do
    return $ graphics
      [ colored (V4 100 200 255 255) $ rectangle (V2 0 0) (V2 200 100)
      ]

main :: IO ()
main = runUI $ do
  setClearColor (V4 240 240 240 255)

  counter <- setup @_ @"counter"
  register counter

  raw <- setup @_ @"raw"
  register $ rawGraphical raw $ graphics $
    [ gridLayout (V2 50 50) $ translate (V2 2 2) $ graphics $
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
      , colored (V4 255 0 0 255) $ text "Hello, World!"
      ]
    , clip (V2 200 40) $ colored (V4 0 0 0 255) $ text "hey youyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"
    ]

  mainloop All

