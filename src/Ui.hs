{-# LANGUAGE OverloadedStrings #-}

module Ui where

import Data.Time

import Brick (BrickEvent (..), EventM, Widget, vBox, zoom)
import Brick.AttrMap (AttrMap, attrMap, attrName)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Forms (Form (formState), focusedFormInputAttr, handleFormEvent, invalidFormInputAttr, renderForm)
import Brick.Main
import Brick.Util (bg, fg, on)
import Brick.Widgets.Border (borderAttr)
import Brick.Widgets.Center (centerLayer, hCenterLayer, vCenterLayer)
import Brick.Widgets.Core (hLimit)
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.ProgressBar (progressCompleteAttr)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Sequence qualified as Seq
import Graphics.Vty as Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import Pomodoro
import Stats
import Tasks
import Util

attrs :: AttrMap
attrs =
  attrMap
    defAttr
    ( [ (attrName "red", fg red)
      , (borderAttr, fg red)
      , (attrName "highlight", fg red)
      , (attrName "accent", fg red)
      , (attrName "highlightBg", bg yellow)
      , (progressCompleteAttr, bg brightRed)
      , (editFocusedAttr, black `on` yellow)
      , (invalidFormInputAttr, black `on` red)
      , (focusedFormInputAttr, black `on` yellow)
      ]
        ++ levelAttrMap
    )

data AppState = AppState
  { _chan :: BChan PomoEvent
  , _sesh :: PomoSession
  , _ts :: TaskState
  , _focus :: AppFocus
  , _taskForm :: Form TaskFormState PomoEvent PomoResource
  , _stats :: StatsState
  , _showingTasks :: Bool
  , _showingStats :: Bool
  , _showingHelp :: Bool
  }

makeLenses ''AppState

unfocus :: EventM PomoResource AppState ()
unfocus = do
  zoom ts $ handleTaskEvent Deselect
  focus .= Pomo

eventHandler :: BrickEvent PomoResource PomoEvent -> EventM PomoResource AppState ()
eventHandler ev = case ev of
  (AppEvent e) -> do
    case e of
      SaveTasks taskList -> saveTs taskList
      TimerEvent task -> do
        zoom sesh $ handlePomoEvent task
        use (ts . tasks) >>= saveTs . toList
      CompleteTask task -> zoom ts $ handleTaskEvent (Append task)
      RefreshStats -> zoom stats $ handleStatsEvent Refresh
  MouseDown n _ _ _ -> case n of
    DayWidget day -> zoom stats $ handleStatsEvent (SelectDay day)
    _ -> pure ()
  (VtyEvent (EvKey k [])) -> do
    use focus >>= \case
      TaskForm -> case k of
        KEsc -> focus .= Pomo
        KEnter -> do
          t <- liftIO getZonedTime
          state <- formState <$> use taskForm
          let updateTime = if state ^. new then timeCreated .~ t else id
              pom = updateTime (state ^. res)
          zoom ts $ handleTaskEvent (Add pom)
          liftIO newTaskForm >>= (taskForm .=)
        _ -> zoom taskForm $ handleFormEvent ev
      f -> case k of
        (KChar 'p') -> use chan >>= zoom sesh . toggleTimer
        (KChar 'q') -> halt
        (KChar 'n') -> use chan >>= zoom sesh . skip
        (KChar 't') -> showingTasks %= not
        (KChar 's') -> showingStats %= not
        (KChar '?') -> showingHelp %= not
        (KChar 'i') -> do
          unfocus
          focus .= TaskForm
          showingTasks .= True
        _ -> case f of
          Tasks -> case k of
            (KChar 'd') -> zoom ts $ handleTaskEvent DeleteSelected
            (KChar 'j') -> zoom ts $ handleTaskEvent SelDown
            (KChar 'k') -> zoom ts $ handleTaskEvent SelUp
            (KChar 'c') -> zoom ts $ handleTaskEvent CompleteSelected
            (KChar 'e') -> do
              focus .= TaskForm
              cur <- zoom ts $ getSelected
              maybe (pure ()) ((taskForm .=) . flip mkTaskForm False) cur
            KEsc -> unfocus
            KEnter -> do
              old <- use (sesh . focusedTask)
              zoom ts $ handleTaskEvent (SelectWithOld old)
              unfocus
            _ -> pure ()
          Pomo -> case k of
            (KChar 'c') -> zoom sesh $ handlePomoEvent Complete
            -- it would technically work to have these be global but clearer this way
            (KChar 'j') -> do
              focus .= Tasks
              zoom ts $ handleTaskEvent SelDown
            (KChar 'k') -> do
              focus .= Tasks
              zoom ts $ handleTaskEvent SelUp
            _ -> pure ()
  _ -> pure ()
 where
  saveTs taskList = use (sesh . focusedTask) >>= liftIO . saveTasks . maybe taskList ((: taskList))

draw :: AppState -> [Widget PomoResource]
draw s =
  let
    ws =
      catMaybes
        [ Just $ hLimit 30 (pomoW $ s ^. sesh)
        , if (s ^. focus == TaskForm)
            then Just $ hLimit 30 $ bord yellow "New task" (renderForm (s ^. taskForm))
            else Nothing
        , if (s ^. showingTasks)
            then Just $ hLimit 30 $ tasksW (s ^. ts)
            else Nothing
        , if (s ^. showingStats)
            then Just $ hLimit 55 $ statsW (s ^. stats)
            else Nothing
        ]
   in
    catMaybes
      [ if s ^. showingHelp then Just (centerLayer . helpW $ s ^. focus) else Nothing
      , Just $ vCenterLayer $ vBox $ map hCenterLayer ws
      ]

app :: App AppState PomoEvent PomoResource
app =
  App
    { appDraw = draw
    , appHandleEvent = eventHandler
    , appStartEvent = do
        vty <- getVtyHandle
        liftIO $ Vty.setMode (Vty.outputIface vty) Vty.Mouse True
    , appAttrMap = const attrs
    , appChooseCursor = showFirstCursor
    }

initApp :: IO AppState
initApp = do
  c <- newBChan 10
  loadedTasks <- loadTasks
  initTaskForm <- newTaskForm
  initStats <- getStats
  let initTs =
        defaultTaskSession
          & (sendTask .~ \t -> writeBChan c (TimerEvent (SelectTask t)))
          & (saveTasks' .~ \xs -> writeBChan c (SaveTasks xs))
          & (tasks .~ Seq.fromList loadedTasks)
      initSesh =
        defaultPomoSession
          & (complete .~ \t -> writeBChan c (CompleteTask t))
          & (Pomodoro.logActivity .~ \a -> liftIO (Util.logActivity a) *> writeBChan c RefreshStats)
  pure
    AppState
      { _chan = c
      , _sesh = initSesh
      , _ts = initTs
      , _showingTasks = True
      , _focus = Pomo
      , _taskForm = initTaskForm
      , _stats = initStats
      , _showingStats = True
      , _showingHelp = False
      }

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
