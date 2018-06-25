{-# LANGUAGE OverloadedStrings #-}
module ClickCounterButton where

import qualified SDL as SDL
import qualified SDL.Primitive as SDLP
import SDL.Vect
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.State
import Data.Extensible
import Data.Ix (inRange)
import qualified Data.Text as T
import qualified Data.Refluxive.UI.Button as Button
import Graphics.UI.Refluxive

instance Component UI "click-counter" where
  type ModelParam "click-counter" = ()
  data Model "click-counter" = ClickCounterModel
    { counter :: Int
    , button :: ComponentView "button"
    }
  data Signal "click-counter"

  newModel () = do
    button <- new @"button" $
      #label @= "Click me!"
      <: #size @= V2 250 40
      <: nil
    register button

    return $ ClickCounterModel
      { counter = 0
      , button = button
      }

  initComponent self = do
    model <- getModel self
    Button.onClick (button model) self $ \_ -> do
      modify $ \model -> model { counter = counter model + 1 }
      c <- fmap counter get
      lift $ operateModel (button model) $ Button.label .= "You clicked " `T.append` T.pack (show c) `T.append` " times!"

  getGraphical model = view $ button model

instance Component UI "app" where
  type ModelParam "app" = ()
  data Model "app" = AppModel { ccbutton :: ComponentView "click-counter" }
  data Signal "app"

  newModel () = do
    ccbutton <- new @"click-counter" ()
    register ccbutton

    return $ AppModel
      { ccbutton = ccbutton
      }

  initComponent self = do
    return ()

  getGraphical model = do
    buttonView <- view $ ccbutton model

    return $ translate (V2 50 50) $ buttonView

main = runUI $ do
  setClearColor (V4 255 255 255 255)

  app <- new @"app" ()
  register app

  mainloop [asRoot app]

