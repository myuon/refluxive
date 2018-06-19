module GameOfLife where

import qualified SDL as SDL
import SDL.Vect
import Control.Lens hiding (view)
import Control.Monad.State
import Data.Ix (inRange)
import Data.Extensible
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

instance Component UI "app" where
  type ModelParam "app" = Record '[ "boardSize" >: V2 Int, "cellSize" >: V2 Int ]
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
    cells <- newCellArray (param ^. #boardSize)
    stepButton <- new @"button" $
      #label @= "Step"
      <: #size @= V2 80 40
      <: nil
    runButton <- new @"button" $
      #label @= "Run"
      <: #size @= V2 80 40
      <: nil
    genButton <- new @"button" $
      #label @= "Generate"
      <: #size @= V2 140 40
      <: nil
    clearButton <- new @"button" $
      #label @= "Clear"
      <: #size @= V2 80 40
      <: nil
    register stepButton
    register runButton
    register genButton
    register clearButton

    return $ AppModel
      { boardSize = param ^. #boardSize
      , cellSize = param ^. #cellSize
      , cellArray = cells
      , margin = V2 50 70
      , stepButton = stepButton
      , runButton = runButton
      , genButton = genButton
      , clearButton = clearButton
      , appState = Stop
      }

  initComponent self = do
    b <- use builtIn

    let model = getModel self
    addWatchSignal self $ watch b $ \_ -> \case
      BuiltInSignal (SDL.Event _ (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (SDL.P v)))) -> do
        let pos = div <$> (fmap fromEnum v - fmap fromEnum (margin model)) <*> (cellSize model)
        when (inRange (0, boardSize model - 1) pos) $ do
          modifyCell (cellArray model) pos not
      TickSignal -> do
        model <- get
        case appState model of
          Running -> step
          _ -> return ()
      _ -> return ()

    addWatchSignal self $ watch (stepButton model) $ \_ -> \case
      Button.Click -> step

    addWatchSignal self $ watch (genButton model) $ \_ -> \case
      Button.Click -> do
        model <- get
        randomGenCellArray (cellArray model)

    addWatchSignal self $ watch (runButton model) $ \_ -> \case
      Button.Click -> do
        model <- get
        case appState model of
          Stop -> do
            lift $ operateModel (runButton model) $ modify $ \m -> m { Button.label = "Stop" }
            put $ model { appState = Running }
          Running -> do
            lift $ operateModel (runButton model) $ modify $ \m -> m { Button.label = "Run" }
            put $ model { appState = Stop }

    addWatchSignal self $ watch (clearButton model) $ \_ -> \case
      Button.Click -> do
        model <- get
        clearCellArray (cellArray model)

  getGraphical model = do
    cellGraphicals <- forM [V2 x y | x <- [0..boardSize model^._x - 1], y <- [0..boardSize model^._y - 1]] $ \v -> do
      cell <- readCell (cellArray model) v

      return $ translate (fmap toEnum v) $
        rectangleWith (#fill @= cell <: nil) (V2 0 0) (V2 1 1)

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
  app <- new @"app" $
    #boardSize @= V2 15 15
    <: #cellSize @= V2 20 20
    <: nil
  register app

  mainloop [asRoot app]

