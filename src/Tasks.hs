{-# LANGUAGE OverloadedStrings #-}

module Tasks where

import Brick
import Brick.Forms
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Maybe (isJust, isNothing)
import Data.Sequence (Seq (..), (<|), (|>))
import Data.Sequence qualified as Seq
import Data.Time (Day, LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getCurrentTime, utctDay)
import Data.Time.LocalTime (getZonedTime)
import Graphics.Vty (Color, white)
import Graphics.Vty.Attributes (brightWhite, yellow)
import Lens.Micro (to, (.~), (^.), (^?), (^?!), _Just)
import Lens.Micro.Mtl (preview, use, view, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import Pomodoro
import Util hiding (saveTasks)

data Control
  = SelUp
  | SelDown
  | Deselect
  | DeleteSelected
  | CompleteSelected
  | SelectWithOld (Maybe Task)
  | Add Task
  | Append Task
  | Save
  | Load [Task]

data TaskState = TaskState
  { _tasks :: Seq Task
  , _selected :: Maybe Int
  , _sendTask :: Task -> IO ()
  , _saveTasks' :: [Task] -> IO ()
  }

exTasks :: TaskState
exTasks =
  TaskState
    { _tasks = Seq.fromList []
    , _selected = Nothing
    , _sendTask = \_ -> pure ()
    , _saveTasks' = \_ -> pure ()
    }

makeLenses ''TaskState

save :: EventM n TaskState ()
save = do
  ts <- use tasks
  f <- use saveTasks'
  liftIO $ f (toList ts)

handleTaskEvent :: Control -> EventM n TaskState ()
handleTaskEvent = \case
  SelUp -> addSel (-1)
  SelDown -> addSel 1
  Deselect -> selected .= Nothing
  DeleteSelected -> do
    use selected >>= maybe (pure ()) ((tasks %=) . Seq.deleteAt)
    save
  CompleteSelected ->
    use selected >>= \case
      Nothing -> pure ()
      Just i -> do
        t <- use tasks <&> (`Seq.index` i)
        tasks %= Seq.deleteAt i
        now <- liftIO getZonedTime
        let newT = (timeFinished .~ Just now) t
        tasks %= (|> newT)
        save
  Add p -> do
    tasks %= (p <|)
    save
  Append p -> do
    tasks %= (|> p)
    save
  SelectWithOld old -> do
    f <- use sendTask
    idx <- use selected
    case idx of
      Just i -> do
        t <- (`Seq.index` i) <$> use tasks
        liftIO (f t)
        tasks %= Seq.deleteAt i
        maybe (pure ()) (\p -> tasks %= (p <|)) old
      Nothing -> pure ()
  Save -> save
  Load ts -> do
    today <- zonedTimeToLocalDay <$> liftIO getZonedTime

    let finishedDay = (preview $ timeFinished . _Just . to zonedTimeToLocalDay)
    let filtered = filter (maybe True (== today) . finishedDay) ts
    tasks .= Seq.fromList filtered
 where
  addSel :: Int -> EventM n TaskState ()
  addSel i = do
    l <- Seq.length <$> use tasks
    when (l > 0) $ do
      cur <- use selected
      selected .= case cur of
        Nothing -> Just 0
        Just n -> Just ((n + i) `mod` l)

col :: Bool -> Color
col focused = if focused then yellow else white

tasksW :: TaskState -> Widget n
tasksW ts =
  let blah = toList $
        flip Seq.mapWithIndex (ts ^. tasks) $
          \i t ->
            padRight Max $
              if ts ^. selected == Just i
                then withAttr (attrName "accent") (txt (taskPretty t))
                else txt (taskPretty t)
   in if null blah
        then txt "no tasks ([i] to insert task)"
        else
          bord (col $ isJust (ts ^. selected)) "tasks" $
            vBox $
              blah

mkTaskForm :: Task -> Form Task PomoEvent PomoResource
mkTaskForm =
  let label s w =
        (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
   in newForm
        [ label "title" @@= editTextField title TaskTitleField (Just 1)
        , label "target" @@= editShowableField target TaskTargetField
        ]

defaultTaskForm :: IO (Form Task PomoEvent PomoResource)
defaultTaskForm = do
  t <- getZonedTime
  pure $ mkTaskForm (Task "" 0 0 t Nothing)
