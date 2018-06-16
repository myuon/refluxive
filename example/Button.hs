{-# LANGUAGE OverloadedStrings #-}
module Button where

import qualified SDL as SDL
import qualified SDL.Primitive as SDLP
import SDL.Vect
import Control.Lens hiding (view)
import Control.Monad
import Control.Monad.State
import Data.Extensible
import Data.Ix (inRange)
import qualified Data.Text as T
import Graphics.UI.Refluxive

data ButtonState = None | Hover | Clicking
  deriving Eq

instance Component UI "button" where
  type ModelParam "button" = Record
    [ "label" >: T.Text
    , "clicked_label" >: (Int -> T.Text)
    , "size" >: SDLP.Pos
    ]

  data Model "button" = ButtonModel
    { label :: T.Text
    , clickedLabel :: Int -> T.Text
    , size :: SDLP.Pos
    , buttonState :: ButtonState
    , clickCounter :: Int
    }

  data Signal "button" = Click

  newModel param = return $ ButtonModel
    { label = param ^. #label
    , clickedLabel = param ^. #clicked_label
    , size = param ^. #size
    , buttonState = None
    , clickCounter = 0
    }

  initComponent self = do
    b <- use builtIn

    addWatchSignal self $ watch b $ \rs -> \case
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        model <- get
        when (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + size model) (fmap fromEnum v)) $ do
          modify $ \model -> model { buttonState = Clicking }
          lift $ emit self Click
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        model <- get
        if (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + size model) (fmap fromEnum v))
          then modify $ \model -> model { buttonState = Hover }
          else modify $ \model -> model { buttonState = None }
      BuiltInSignal (SDL.Event _ (SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ (SDL.P v) _))) -> do
        model <- get
        if (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + size model) (fmap fromEnum v))
          then modify $ \model -> model { buttonState = Hover }
          else modify $ \model -> model { buttonState = None }
      _ -> return ()
    addWatchSignal self $ watch self $ \_ -> \case
      Click -> do
        modify $ \model -> model { clickCounter = clickCounter model + 1 }

  getGraphical model = do
    return $ graphics $
      [ colored (if buttonState model == Hover then V4 220 220 220 255 else V4 200 200 200 255) $ rectangle (V2 0 0) (size model)
      , colored (V4 0 0 0 255) $ rectangleWith (#fill @= False <: nil) (V2 0 0) (size model)
      , translate (V2 10 5) $ text $ if clickCounter model == 0 then label model else clickedLabel model (clickCounter model)
      ]

instance Component UI "app" where
  type ModelParam "app" = ()
  data Model "app" = AppModel { button :: ComponentView "button" }
  data Signal "app"

  newModel () = do
    button <- new @"button" $
      #label @= "Click me!"
      <: #clicked_label @= (\n -> "You clicked " `T.append` T.pack (show n) `T.append` " times")
      <: #size @= V2 250 40
      <: nil
    register button

    return $ AppModel
      { button = button
      }

  initComponent self = do
    return ()

  getGraphical model = do
    buttonView <- view $ button model

    return $ translate (V2 50 50) $ buttonView

main = runUI $ do
  setClearColor (V4 255 255 255 255)

  app <- new @"app" ()
  register app

  mainloop [asRoot app]

