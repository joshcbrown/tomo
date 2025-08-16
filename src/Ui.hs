{-# LANGUAGE OverloadedStrings #-}

module Ui where

import Data.Time

import Brick (BrickEvent (..), EventM, Widget, zoom)
import Brick.AttrMap (AttrMap, attrMap, attrName)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Forms (Form (formState), focusedFormInputAttr, handleFormEvent, invalidFormInputAttr, renderForm)
import Brick.Main
import Brick.Util (bg, fg, on)
import Brick.Widgets.Border (borderAttr)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (hLimit, (<=>))
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.ProgressBar (progressCompleteAttr)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Graphics.Vty as Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import Pomodoro
import Stats
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
  , _taskForm :: Form Task PomoEvent PomoResource
  , _stats :: StatsState
  }

makeLenses ''AppState

unfocus :: EventM PomoResource AppState ()
unfocus = do
  zoom ts $ handleTaskEvent Deselect
  focus .= Unfocused

eventHandler :: BrickEvent PomoResource PomoEvent -> EventM PomoResource AppState ()
eventHandler ev = case ev of
  (AppEvent t) -> do
    case t of
      SaveTasks taskList -> saveTs taskList
      TimerEvent t -> do
        zoom sesh $ handle t
        use (ts . tasks) >>= saveTs . toList
      CompleteTask t -> zoom ts $ handleTaskEvent (Append t)
      RefreshStats -> zoom stats $ refreshState
  (VtyEvent (EvKey k [])) -> do
    use focus >>= \case
      TaskForm -> case k of
        KEsc -> focus .= Unfocused
        KEnter -> do
          t <- liftIO getZonedTime
          pom <- (timeCreated .~ t) . formState <$> use taskForm
          zoom ts $ handleTaskEvent (Add pom)
          liftIO defaultTaskForm >>= (taskForm .=)
        _ -> zoom taskForm $ handleFormEvent ev
      f -> case k of
        (KChar 'p') -> use chan >>= zoom sesh . toggleTimer
        (KChar 'q') -> halt
        (KChar 'n') -> use chan >>= zoom sesh . skip
        (KChar 't') -> showingTasks %= not
        (KChar 'i') -> do
          unfocus
          focus .= TaskForm
          showingTasks .= True
        _ -> case f of
          Tasks -> case k of
            (KChar 'd') -> zoom ts $ handleTaskEvent Delete
            (KChar 'j') -> zoom ts $ handleTaskEvent SelDown
            (KChar 'k') -> zoom ts $ handleTaskEvent SelUp
            KEsc -> unfocus
            KEnter -> do
              old <- use (sesh . task)
              zoom ts $ handleTaskEvent (SelectWithOld old)
              unfocus
            _ -> pure ()
          Unfocused -> case k of
            (KChar 'j') -> do
              focus .= Tasks
              zoom ts $ handleTaskEvent SelDown
            (KChar 'k') -> do
              focus .= Tasks
              zoom ts $ handleTaskEvent SelUp
            (KChar 'c') -> zoom sesh $ handle Complete
            _ -> pure ()
  _ -> pure ()
 where
  saveTs taskList = use (sesh . task) >>= liftIO . saveTasks . maybe taskList ((: taskList))

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
    fourth = third <=> statsW (s ^. stats)
   in
    [center $ hLimit 30 fourth]

app :: App AppState PomoEvent PomoResource
app =
  App
    { appDraw = draw
    , appHandleEvent = eventHandler
    , appStartEvent = do
        -- TODO: move all this shit to initApp
        c <- use chan
        ts . sendTask .= \t -> writeBChan c (TimerEvent (SelectTask t))
        ts . saveTasks' .= \xs -> writeBChan c (SaveTasks xs)
        sesh . complete .= \t -> writeBChan c (CompleteTask t)
        sesh . Pomodoro.logActivity .= \a -> liftIO (Util.logActivity a) *> writeBChan c RefreshStats
        liftIO loadTasks >>= zoom ts . handleTaskEvent . Load
    , appAttrMap = const attrs
    , appChooseCursor = neverShowCursor
    }

initApp :: IO AppState
initApp =
  AppState
    <$> newBChan 10
    <*> pure ex
    <*> pure exTasks
    <*> pure True
    <*> pure Unfocused
    <*> defaultTaskForm
    <*> getStats

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
