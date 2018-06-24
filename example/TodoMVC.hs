{-# LANGUAGE OverloadedStrings #-}
module TodoMVC where

import qualified SDL as SDL
import SDL.Vect
import Control.Monad
import Control.Monad.State
import Control.Lens hiding (view)
import Data.Extensible
import Data.Ix (inRange)
import qualified Data.Text as T
import Graphics.UI.Refluxive

instance Component UI "text-form" where
  type ModelParam "text-form" = Record '[ "placeholder" >: T.Text ]
  data Model "text-form" = TextformModel
    { content :: T.Text
    , placeholder :: T.Text
    }
  data Signal "text-form" = CreateItem T.Text

  newModel p = return (TextformModel "" (p ^. #placeholder))

  initComponent self = do
    b <- use _builtIn

    addWatchSignal self $ watch b $ \_ -> \case
      BuiltInSignal (SDL.Event _ (SDL.TextInputEvent (SDL.TextInputEventData _ txt))) -> do
        modify $ (\model -> model { content = content model `T.append` txt })
      BuiltInSignal (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeBackspace _)))) -> do
        modify $ (\model -> model { content = if T.null (content model) then content model else T.init (content model) })
      BuiltInSignal (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeReturn _)))) -> do
        model <- get
        lift $ emit self $ CreateItem (content model)
        put $ model { content = "" }
      _ -> return ()

  getGraphical (TextformModel txt placeholder) = do
    return $ clip (V2 300 50) $ graphics
      [ colored (V4 200 200 200 255) $ rectangleWith (#fill @= False <: nil) (V2 0 0) (V2 300 50)
      , translate (V2 5 13) $
        if T.null txt
        then colored (V4 100 100 100 255) $ text $ placeholder
        else colored (V4 0 0 0 255) $ text $ txt `T.append` "■"
      ]

instance Component UI "checkbox" where
  type ModelParam "checkbox" = ()
  data Model "checkbox" = CheckBoxModel Bool
  data Signal "checkbox" = Changed Bool

  newModel () = return $ CheckBoxModel False

  initComponent self = do
    b <- use _builtIn

    addWatchSignal self $ watch b $ \rs -> \case
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        when (inRange (fmap fromEnum $ coordinate rs, fmap fromEnum $ coordinate rs + V2 30 30) (fmap fromEnum v)) $ do
          modify $ \(CheckBoxModel b) -> CheckBoxModel (not b)

          CheckBoxModel b <- get
          lift $ emit self $ Changed b
      _ -> return ()

  getGraphical (CheckBoxModel b) = do
    return $ graphics
      [ if b
        then colored (V4 50 150 50 255) $ text "✓"
        else colored (V4 200 200 200 255) $ text "□"
      ]

getCheckState :: MonadIO m => ComponentView "checkbox" -> m Bool
getCheckState m = fmap (\(CheckBoxModel b) -> b) $ getModel m

instance Component UI "item-checklist" where
  type ModelParam "item-checklist" = ()
  data Model "item-checklist" = ItemListModel [(ComponentView "checkbox", T.Text)]
  data Signal "item-checklist" = AddItem T.Text

  newModel () = return $ ItemListModel []

  initComponent self = do
    addWatchSignal self $ watch self $ \_ -> \case
      AddItem item -> do
        checkbox <- lift $ new @"checkbox" ()
        lift $ register checkbox

        modify $ \(ItemListModel xs) -> ItemListModel ((checkbox, item) : xs)

  getGraphical (ItemListModel xs) = do
    fmap graphics $ forM (zip [0..] xs) $ \(i, (checkbox, content)) -> do
      checkboxView <- view $ checkbox
      checkState <- getCheckState checkbox

      return $ translate (V2 0 (i * 30)) $ graphics
        [ translate (V2 0 0) $ checkboxView
        , translate (V2 30 0) $
          colored (if checkState then V4 255 100 100 255 else V4 0 0 0 255) $ text content
        ]

instance Component UI "app" where
  type ModelParam "app" = ()
  data Model "app" = AppModel
    { textform :: ComponentView "text-form"
    , itemlist :: ComponentView "item-checklist"
    }
  data Signal "app"

  newModel () = do
    textform <- new @"text-form" (#placeholder @= "What needs to be done?" <: nil)
    itemlist <- new @"item-checklist" ()

    register textform
    register itemlist

    return $ AppModel
      { textform = textform
      , itemlist = itemlist
      }

  initComponent self = do
    b <- use _builtIn
    model <- getModel self

    addWatchSignal self $ watch (textform model) $ \_ -> \case
      CreateItem item -> do
        model <- get
        lift $ emit (itemlist model) $ AddItem item

    addWatchSignal self $ watch b $ \rs -> \case
      BuiltInSignal (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ SDL.KeycodeEscape _)))) ->
        lift quit
      _ -> return ()

  getGraphical model = do
    textformView <- view $ textform model
    itemlistView <- view $ itemlist model

    return $ translate (V2 50 50) $ graphics $
      [ textformView
      , translate (V2 0 50) itemlistView
      ]

main :: IO ()
main = runUI $ do
  setClearColor (V4 255 255 255 255)

  app <- new @"app" ()
  register app

  mainloop $ [asRoot app]

