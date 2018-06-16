{-# LANGUAGE OverloadedStrings #-}
module TodoMVC where

import qualified SDL as SDL
import SDL.Vect
import Control.Monad
import Control.Monad.State
import Data.Extensible
import qualified Data.Text as T
import Graphics.UI.Refluxive

instance Component UI "text-form" where
  data Model "text-form" = TextformModel
    { content :: T.Text
    , placeholder :: T.Text
    }
  data Signal "text-form" = CreateItem T.Text

  watcher _ =
    [ watch "builtin" $ \case
        BuiltInSignal (SDL.Event _ (SDL.TextInputEvent (SDL.TextInputEventData _ txt))) -> do
          modify $ (\model -> model { content = content model `T.append` txt })
        BuiltInSignal (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeBackspace _)))) -> do
          modify $ (\model -> model { content = if T.null (content model) then content model else T.init (content model) })
        BuiltInSignal (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeReturn _)))) -> do
          model <- get
          lift $ emit $ CreateItem (content model)
          put $ model { content = "" }
        _ -> return ()
    ]

  setup = new (TextformModel "" "What needs to be done?")

  getGraphical (TextformModel txt placeholder) =
    return $ clip (V2 200 50) $ graphics
      [ colored (V4 200 200 200 255) $ rectangleWith (#fill @= False <: nil) (V2 0 0) (V2 200 50)
      , translate (V2 5 13) $
        if T.null txt
        then colored (V4 100 100 100 255) $ text $ placeholder
        else colored (V4 0 0 0 255) $ text $ txt `T.append` "■"
      ]

instance Component UI "button" where
  data Model "button" = ButtonModel Bool T.Text
  data Signal "button" = Clicked | Hover

  watcher _ =
    [ watch "builtin" $ \case
        BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (SDL.P v)))) -> do
          liftIO $ print v
        _ -> return ()
    ]

  setup = new $ ButtonModel False "button"

  getGraphical (ButtonModel b txt) =
    return $ graphics
      [ colored (V4 40 40 40 255) $ rectangleWith (#fill @= True <: nil) (V2 0 0) (V2 50 30)
      , colored (V4 255 255 255 255) $ translate (V2 5 5) $ text txt
      ]

instance Component UI "item-list" where
  data Model "item-list" = ItemListModel [T.Text]
  data Signal "item-list" = AddItem T.Text

  setup = new $ ItemListModel []

  getGraphical (ItemListModel xs) =
    return $ graphics $
      fmap (\(i,x) -> translate (V2 0 (i * 30)) $ text x) $ zip [0..] xs

instance Component UI "app" where
  data Model "app" = AppModel
    { textform :: ComponentView "text-form"
    , itemlist :: ComponentView "item-list"
    , button :: ComponentView "button"
    }

  watcher _ =
    [ watch "text-form" $ \case
        CreateItem item -> do
          model <- get
          lift $ operateModel (itemlist model) $ do
            modify $ \(ItemListModel xs) -> ItemListModel (item : xs)
    , watch "builtin" $ \case
        BuiltInSignal (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeEscape _)))) ->
          lift quit
        _ -> return ()
    ]

  setup = do
    textform <- setup @_ @"text-form"
    itemlist <- setup @_ @"item-list"
    button <- setup @_ @"button"

    register textform
    register itemlist
    register button

    new $ AppModel
      { textform = textform
      , itemlist = itemlist
      , button = button
      }

  getGraphical model = do
    textformView <- view $ textform model
    itemlistView <- view $ itemlist model
    buttonView <- view $ button model

    return $ translate (V2 50 50) $ graphics $
      [ textformView
      , translate (V2 0 50) itemlistView
      , translate (V2 0 200) buttonView
      ]

main :: IO ()
main = runUI $ do
  setClearColor (V4 255 255 255 255)

  app <- setup @_ @"app"
  register app

  mainloop $ RootUIDs [uid app]

