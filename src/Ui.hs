{-# LANGUAGE OverloadedStrings #-}

module Ui where

import Data.Time
import Graphics.Vty (
  black,
  brightRed,
  brightWhite,
  cyan,
  defaultConfig,
  green,
  red,
  white,
  withURL,
  yellow,
 )

import Brick (BrickEvent (..), EventM, Padding (..), Widget, modify, zoom)
import Brick.AttrMap (AttrMap, attrMap, attrName)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Forms (Form (formState), focusedFormInputAttr, handleFormEvent, invalidFormInputAttr, renderForm)
import Brick.Main
import Brick.Util (bg, fg, on)
import Brick.Widgets.Border (border, borderAttr)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (hLimit, padTop, (<=>))
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Brick.Widgets.ProgressBar (progressCompleteAttr)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence (Seq (..))
import Graphics.Vty as Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import Pomodoro
import Tasks
import Util

attrs :: AttrMap
attrs =
  attrMap
    (fg brightWhite)
    [ (attrName "red", fg red)
    , (borderAttr, fg red)
    , (attrName "highlight", fg red)
    , (attrName "accent", fg red)
    , (progressCompleteAttr, bg brightRed)
    , (editFocusedAttr, black `on` yellow)
    , (invalidFormInputAttr, black `on` red)
    , (focusedFormInputAttr, black `on` yellow)
    ]

data AppState = AppState
  { _chan :: BChan PomoEvent
  , _sesh :: PomoSession
  , _ts :: TaskState
  , _showingTasks :: Bool
  , _focus :: AppFocus
  , _taskForm :: Form Pomo PomoEvent PomoResource
  }

makeLenses ''AppState

unfocus :: EventM PomoResource AppState ()
unfocus = do
  zoom ts $ handleTaskEvent Deselect
  focus .= Unfocused

eventHandler :: BrickEvent PomoResource PomoEvent -> EventM PomoResource AppState ()
eventHandler ev = case ev of
  (AppEvent t) -> zoom sesh $ handle t
  (VtyEvent (EvKey k [])) -> do
    use focus >>= \case
      TaskForm -> case k of
        KEsc -> focus .= Unfocused
        KEnter -> do
          pom <- formState <$> use taskForm
          taskForm .= defaultTaskForm
          zoom ts $ handleTaskEvent (Add pom)
        _ -> zoom taskForm $ handleFormEvent ev
      Tasks -> case k of
        (KChar 'j') -> do
          zoom ts $ handleTaskEvent SelDown
        (KChar 'k') -> do
          zoom ts $ handleTaskEvent SelUp
        (KChar 't') -> unfocus *> (showingTasks %= not)
        KEsc -> unfocus
        KEnter -> do
          old <- use (sesh . pomo)
          zoom ts $ handleTaskEvent (SelectWithOld old)
          unfocus
        _ -> pure ()
      Unfocused -> case k of
        (KChar 'p') -> use chan >>= zoom sesh . toggleTimer
        (KChar 'q') -> halt
        (KChar 'n') -> use chan >>= zoom sesh . skip
        (KChar 'j') -> do
          focus .= Tasks
          zoom ts $ handleTaskEvent SelDown
        (KChar 'k') -> do
          focus .= Tasks
          zoom ts $ handleTaskEvent SelUp
        (KChar 't') -> showingTasks %= not
        (KChar 'i') -> do
          unfocus
          focus .= TaskForm
          showingTasks .= True
        (KChar 'c') -> sesh . pomo .= Nothing
        _ -> pure ()
  _ -> pure ()

draw :: AppState -> [Widget PomoResource]
draw s =
  let
    first = pomoW (s ^. sesh)
    second = case (s ^. focus) of
      TaskForm -> first <=> bord yellow "new task" (renderForm (s ^. taskForm))
      _ -> first
    third =
      if (s ^. showingTasks)
        then second <=> tasksW (s ^. ts)
        else second
   in
    [center $ hLimit 30 third]

app :: App AppState PomoEvent PomoResource
app =
  App
    { appDraw = draw
    , appHandleEvent = eventHandler
    , appStartEvent = do
        c <- use chan
        zoom sesh $ startTimer c
        ts . sendTask .= \t -> writeBChan c (SelectTask t)
        focus .= Unfocused
        zoom ts $ handleTaskEvent Deselect
    , appAttrMap = const attrs
    , appChooseCursor = neverShowCursor
    }

initApp :: IO AppState
initApp =
  AppState
    <$> newBChan 10
    <*> pure ex
    <*> pure exTasks
    <*> pure False
    <*> pure Unfocused
    <*> pure defaultTaskForm

appMain :: IO ()
appMain = do
  s <- initApp
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  pure ()
    <* customMain
      initialVty
      buildVty
      (Just $ s ^. chan)
      app
      s
