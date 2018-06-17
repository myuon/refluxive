module GameOfLife where

import qualified SDL as SDL
import SDL.Vect
import Control.Lens hiding (view)
import Control.Monad.State
import Data.Extensible
import Data.IORef
import Data.Ix (inRange)
import qualified Data.Vector.Mutable as V
import qualified Data.Refluxive.UI.Button as Button
import Graphics.UI.Refluxive

type CellArray = V.IOVector (V.IOVector Bool)

newCellArray :: MonadIO m => V2 Int -> Bool -> m CellArray
newCellArray v b = liftIO $ do
  V.replicateM (v ^. _x) $ do
    v <- V.new (v ^. _y)
    V.set v b
    return v

readCell :: (MonadIO m) => CellArray -> V2 Int -> m Bool
readCell arr v = liftIO $ (\vec -> V.read vec (v ^. _y)) =<< V.read arr (v ^. _x)

modifyCell :: (MonadIO m) => CellArray -> V2 Int -> (Bool -> Bool) -> m ()
modifyCell arr v f = liftIO $ do
  vec <- V.read arr (v ^. _x)
  V.modify vec f (v ^. _y)

cloneCellArray :: MonadIO m => CellArray -> m CellArray
cloneCellArray arr = liftIO $ do
  index <- newIORef 0
  V.replicateM (V.length arr) $ do
    i <- readIORef index
    v <- V.read arr i
    modifyIORef index (+1)
    V.clone v

instance Component UI "app" where
  type ModelParam "app" =
    Record '[ "boardSize" >: V2 Int, "cellSize" >: V2 Int ]

  data Model "app" = AppModel
    { board :: CellArray
    , boardSize :: V2 Int
    , cellSize :: V2 Int
    , margin :: V2 Int
    , stepButton :: ComponentView "button"
    }
  data Signal "app"

  newModel p = do
    let size = p ^. #boardSize
    vec <- newCellArray size False

    stepButton <- new @"button" $
      #label @= "Step"
      <: #size @= V2 100 40
      <: nil
    register stepButton

    return $ AppModel
      { board = vec
      , boardSize = size
      , cellSize = p ^. #cellSize
      , margin = V2 100 100
      , stepButton = stepButton
      }

  initComponent self = do
    b <- use builtIn

    addWatchSignal self $ watch b $ \_ -> \case
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        model <- get
        let mp = fmap fromEnum v - margin model
        let pos = V2 (mp^._x `div` cellSize model^._x) (mp^._y `div` cellSize model^._y)
        when (0 <= pos && pos < boardSize model - 1) $ liftIO $ do
          modifyCell (board model) pos not
      _ -> return ()

    let model = getModel self
    addWatchSignal self $ watch (stepButton model) $ \_ -> \case
      Button.Click -> do
        model <- get
        prev <- cloneCellArray (board model)
        forM_ [(x,y) | x <- [0..boardSize model^._x - 1], y <- [0..boardSize model^._y - 1]] $ \(x,y) -> lift $ do
          neighbors <- mapM (readCell prev) [V2 (x+dx) (y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0), inRange (0,boardSize model - 1) (V2 (x+dx) (y+dy))]
          modifyCell (board model) (V2 x y) $ \current -> case (current, length (filter id neighbors)) of
            (False, 3) -> True
            (True, 2) -> True
            (True, 3) -> True
            _ -> False

  getGraphical model = do
    boardGraphical <- forM [(x,y) | x <- [0..boardSize model^._x - 1], y <- [0..boardSize model^._y - 1]] $ \(x,y) -> do
      active <- readCell (board model) (V2 x y)

      return $ colored (V4 0 0 0 255) $ translate (V2 (toEnum x) (toEnum y)) $
        rectangleWith (#fill @= active <: nil) (V2 0 0) (V2 1 1)

    stepBtn <- view $ stepButton model

    return $ graphics
      [ translate (V2 50 50) $ text "Game of Life"
      , translate (fmap toEnum $ margin model) $ gridLayout (fmap toEnum $ V2 (cellSize model ^. _x) (cellSize model ^. _y)) $ graphics $ boardGraphical
      , translate (V2 200 400) $ stepBtn
      ]

main = runUI $ do
  setClearColor (V4 255 255 255 255)
  app <- new @"app" (#boardSize @= V2 15 15 <: #cellSize @= V2 20 20 <: nil)
  register app

  mainloop [asRoot app]

