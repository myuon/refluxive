module GameOfLife where

import qualified SDL as SDL
import SDL.Vect
import Control.Lens hiding (view)
import Control.Monad.State
import Data.Ix (inRange)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Refluxive.UI.Button as Button
import Graphics.UI.Refluxive
import System.Random.MWC

data CellArray = CellArray
  { getCellArray :: VM.IOVector Bool
  , arraySize :: V2 Int
  }

newCellArray :: MonadIO m => V2 Int -> m CellArray
newCellArray size = liftIO $ do
  vec <- VM.new (size^._x * size^._y)
  VM.set vec False
  return $ CellArray vec size

readCell :: MonadIO m => CellArray -> V2 Int -> m Bool
readCell (CellArray arr size) v = liftIO $ do
  let index = v^._x + v^._y * (size^._x)
  VM.read arr index

modifyCell :: MonadIO m => CellArray -> V2 Int -> (Bool -> Bool) -> m ()
modifyCell (CellArray arr size) v f = liftIO $ do
  let index = v^._x + v^._y * (size^._x)
  VM.modify arr f index

cloneCellArray :: MonadIO m => CellArray -> m CellArray
cloneCellArray (CellArray arr size) = liftIO $ VM.clone arr >>= \arr' -> return (CellArray arr' size)

clearCellArray :: MonadIO m => CellArray -> m ()
clearCellArray (CellArray arr size) = liftIO $ VM.set arr False

randomGenCellArray :: MonadIO m => CellArray -> m ()
randomGenCellArray (CellArray arr size) = liftIO $ withSystemRandom $ \gen -> do
  vec <- uniformVector gen (VM.length arr `div` 5) :: IO (V.Vector Int)
  forM_ (V.toList vec) $ \v -> do
    VM.modify arr not (v `mod` VM.length arr)

data AppState = Stop | Running

step :: StateT (Model "app") UI ()
step = do
  model <- get
  prev <- cloneCellArray (cellArray model)

  forM_ [V2 x y | x <- [0..boardSize model^._x - 1], y <- [0..boardSize model^._y - 1]] $ \v -> do
    neighbors <- mapM (readCell prev)
      [v + V2 dx dy | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0), inRange (0, boardSize model - 1) (v + V2 dx dy)]
    modifyCell (cellArray model) v $ \current -> case (current, length (filter id neighbors)) of
      (False, 3) -> True
      (True, n) | n == 2 || n == 3 -> True
      _ -> False

pattern ModelParam :: V2 Int -> V2 Int -> ModelParam "app"
pattern ModelParam{ _boardSize, _cellSize } = (_boardSize, _cellSize)

instance Component UI "app" where
  type ModelParam "app" = (V2 Int, V2 Int)
  data Model "app" = AppModel
    { boardSize :: V2 Int
    , cellSize :: V2 Int
    , cellArray :: CellArray
    , margin :: V2 Int
    , stepButton :: ComponentView "button"
    , runButton :: ComponentView "button"
    , genButton :: ComponentView "button"
    , clearButton :: ComponentView "button"
    , appState :: AppState
    }
  data Signal "app"

  newModel param = do
    cells <- newCellArray (_boardSize param)
    stepButton <- new @"button" $ Button.ModelParam
      { Button.label_ = "Step"
      , Button.size_ = V2 80 40
      }
    runButton <- new @"button" $ Button.ModelParam
      { Button.label_ = "Run"
      , Button.size_ = V2 80 40
      }
    genButton <- new @"button" $ Button.ModelParam
      { Button.label_ = "Generate"
      , Button.size_ = V2 80 40
      }
    clearButton <- new @"button" $ Button.ModelParam
      { Button.label_ = "Clear"
      , Button.size_ = V2 80 40
      }
    register stepButton
    register runButton
    register genButton
    register clearButton

    return $ AppModel
      { boardSize = _boardSize param
      , cellSize = _cellSize param
      , cellArray = cells
      , margin = V2 50 70
      , stepButton = stepButton
      , runButton = runButton
      , genButton = genButton
      , clearButton = clearButton
      , appState = Stop
      }

  initComponent self = do
    b <- use _builtIn

    model <- getModel self
    addWatchSignal self $ watch b $ \_ -> \case
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        model <- getModel self
        let pos = div <$> (fmap fromEnum v - fmap fromEnum (margin model)) <*> (cellSize model)
        when (inRange (0, boardSize model - 1) pos) $ do
          modifyCell (cellArray model) pos not
      TickSignal -> do
        model <- get
        case appState model of
          Running -> step
          _ -> return ()
      _ -> return ()

    Button.onClick (stepButton model) self $ \_ -> step

    Button.onClick (genButton model) self $ \_ -> do
      model <- get
      randomGenCellArray (cellArray model)

    Button.onClick (runButton model) self $ \_ -> do
      model <- get
      case appState model of
        Stop -> do
          lift $ operateModel (runButton model) $ Button.label .= "Stop"
          put $ model { appState = Running }
        Running -> do
          lift $ operateModel (runButton model) $ Button.label .= "Run"
          put $ model { appState = Stop }

    Button.onClick (clearButton model) self $ \_ -> do
      model <- get
      clearCellArray (cellArray model)

  getGraphical model = do
    cellGraphicals <- forM [V2 x y | x <- [0..boardSize model^._x - 1], y <- [0..boardSize model^._y - 1]] $ \v -> do
      cell <- readCell (cellArray model) v

      return $ translate (fmap toEnum v) $
        rectangleWith (ShapeStyle { fill = cell, rounded = Nothing }) (V2 0 0) (V2 1 1)

    stepButtonView <- view $ stepButton model
    runButtonView <- view $ runButton model
    genButtonView <- view $ genButton model
    clearButtonView <- view $ clearButton model

    return $ graphics $
      [ translate (V2 20 20) $ text "Game of Life"
      , translate (fmap toEnum $ margin model) $ gridLayout (fmap toEnum $ cellSize model) $ graphics cellGraphicals
      , translate (V2 20 400) $ stepButtonView
      , translate (V2 110 400) $ runButtonView
      , translate (V2 200 400) $ genButtonView
      , translate (V2 350 400) $ clearButtonView
      ]

main = runUI $ do
  app <- new @"app" $ ModelParam
    { _boardSize = V2 15 15
    , _cellSize = V2 20 20
    }
  register app

  mainloop [asRoot app]

