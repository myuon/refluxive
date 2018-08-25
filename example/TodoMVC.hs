{-# LANGUAGE OverloadedStrings #-}
module TodoMVC where

import qualified SDL as SDL
import SDL.Vect
import Control.Monad
import Control.Monad.State
import Control.Lens hiding (view)
import Data.Ix (inRange)
import qualified Data.Text as T
import qualified Data.Refluxive.UI.Checkbox as Checkbox
import Graphics.UI.Refluxive

pattern ModelParam :: T.Text -> ModelParam "text-form"
pattern ModelParam{ _placeholder } = _placeholder

instance Component UI "text-form" where
  type ModelParam "text-form" = T.Text
  data Model "text-form" = TextformModel
    { content :: T.Text
    , placeholder :: T.Text
    }
  data Signal "text-form" = CreateItem T.Text

  newModel p = return (TextformModel "" (_placeholder p))

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
      [ colored (V4 200 200 200 255) $ rectangleWith (def { fill = False }) (V2 0 0) (V2 300 50)
      , translate (V2 5 13) $
        if T.null txt
        then colored (V4 100 100 100 255) $ textDef placeholder
        else colored (V4 0 0 0 255) $ textDef $ txt `T.append` "■"
      ]

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
      checkState <- fmap (^. Checkbox.checkState) $ getModel checkbox

      return $ translate (V2 0 (i * 30)) $ graphics
        [ translate (V2 0 0) $ checkboxView
        , translate (V2 30 0) $
          if checkState
          then colored (V4 100 100 100 255) $ textDefWith (TextStyle { styles = [Strikethrough] }) content
          else colored (V4 0 0 0 255) $ textDef content
        ]

getItemCount :: MonadIO m => ComponentView "item-checklist" -> m Int
getItemCount m = fmap (\(ItemListModel xs) -> length xs) $ getModel m

getUndoneItemCount :: MonadIO m => ComponentView "item-checklist" -> m Int
getUndoneItemCount m = do
  ItemListModel model <- getModel m
  fmap length $ filterM (\(c,_) -> fmap (^. Checkbox.checkState) $ getModel c) model

getDoneItemCount :: MonadIO m => ComponentView "item-checklist" -> m Int
getDoneItemCount m = do
  ItemListModel model <- getModel m
  fmap length $ filterM (\(c,_) -> fmap (not . (^. Checkbox.checkState)) $ getModel c) model

instance Component UI "app" where
  type ModelParam "app" = ()
  data Model "app" = AppModel
    { textform :: ComponentView "text-form"
    , itemlist :: ComponentView "item-checklist"
    }
  data Signal "app"

  newModel () = do
    textform <- new @"text-form" (ModelParam { _placeholder = "What needs to be done?" })
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

    itemCount <- getItemCount (itemlist model)
    doneItemCount <- getDoneItemCount (itemlist model)

    return $ translate (V2 50 50) $ graphics $
      [ textformView
      , translate (V2 5 60) itemlistView
      , if doneItemCount > 0
        then translate (V2 0 (60 + toEnum itemCount * 30)) $ textDef $ T.pack (show doneItemCount) `T.append` " items left"
        else empty
      ]

main :: IO ()
main = runUI $ do
  setClearColor (V4 255 255 255 255)

  app <- new @"app" ()
  register app

  mainloop $ [asRoot app]

