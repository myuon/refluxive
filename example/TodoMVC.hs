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
  data Model "text-form" = TextformModel T.Text
  data Signal "text-form" = CreateItem T.Text

  watcher _ =
    [ watch "builtin" $ \case
        BuiltInSignal (SDL.Event _ (SDL.TextInputEvent (SDL.TextInputEventData _ txt))) -> do
          modify $ (\(TextformModel t) -> TextformModel $ t `T.append` txt)
        BuiltInSignal (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeBackspace _)))) -> do
          modify $ (\(TextformModel t) -> TextformModel $ if T.null t then t else T.init t)
        BuiltInSignal (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeReturn _)))) -> do
          TextformModel item <- get
          lift $ emit $ CreateItem item
          put $ TextformModel ""
        _ -> return ()
    ]

  setup = new (TextformModel "")

  getGraphical (TextformModel txt) =
    return $ clip (V2 200 100) $ graphics
      [ colored (V4 200 200 200 255) $ rectangleWith (#fill @= False <: nil) (V2 0 0) (V2 200 100)
      , translate (V2 20 20) $ text $ txt `T.append` "■"
      ]

instance Component UI "item-list" where
  data Model "item-list" = ItemListModel [T.Text]
  data Signal "item-list" = AddItem T.Text

  setup = new $ ItemListModel ["----"]

  getGraphical (ItemListModel xs) =
    return $ graphics $
      fmap (\(i,x) -> translate (V2 0 (i * 30)) $ text x) $ zip [0..] xs

instance Component UI "app" where
  data Model "app" = AppModel
    { textform :: ComponentView "text-form"
    , itemlist :: ComponentView "item-list"
    }

  watcher _ =
    [ watch "text-form" $ \case
        CreateItem item -> do
          model <- get
          lift $ operateModel (itemlist model) $ do
            modify $ \(ItemListModel xs) -> ItemListModel (item : xs)
    ]

  setup = do
    textform <- setup @_ @"text-form"
    itemlist <- setup @_ @"item-list"

    register textform
    register itemlist

    new $ AppModel
      { textform = textform
      , itemlist = itemlist
      }

  getGraphical model = do
    textformView <- view $ textform model
    itemlistView <- view $ itemlist model

    return $ translate (V2 50 50) $ graphics $
      [ textformView
      , translate (V2 0 100) itemlistView
      ]

main :: IO ()
main = runUI $ do
  setClearColor (V4 255 255 255 255)

  app <- setup @_ @"app"
  register app

  mainloop $ RootUIDs [uid app]

