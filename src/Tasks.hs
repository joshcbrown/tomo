{-# LANGUAGE OverloadedStrings #-}

module Tasks where

import Brick
import Brick.Forms
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Sequence (Seq (..), (<|), (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Data.Time.LocalTime (getZonedTime)
import Debug.Trace (trace)
import Graphics.Vty (Color, white)
import Graphics.Vty.Attributes (yellow)
import Lens.Micro (to, (.~), (^.), _Just)
import Lens.Micro.Mtl (preview, use, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Type (Lens')
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Util hiding (saveTasks)

data TaskControl
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
  | DeleteAt Int
  | InsertAt Int Task
  | SelectIndex Int

data TaskState = TaskState
  { _tasks :: Seq Task
  , _selectedTask :: Maybe Int
  , _sendTask :: Task -> IO ()
  , _saveTasks' :: [Task] -> IO ()
  }

defaultTaskSession :: TaskState
defaultTaskSession =
  TaskState
    { _tasks = Seq.fromList []
    , _selectedTask = Nothing
    , _sendTask = \_ -> pure ()
    , _saveTasks' = \_ -> pure ()
    }

makeLenses ''TaskState

getSelected :: EventM PomoResource TaskState (Maybe (Int, Task))
getSelected = do
  mi <- use selectedTask
  ts <- use tasks
  pure . flip fmap mi $ \i -> (i, Seq.index ts i)

getSelectedIndex :: EventM PomoResource TaskState (Maybe Int)
getSelectedIndex = use selectedTask

handleTaskEvent :: TaskControl -> EventM PomoResource TaskState ()
handleTaskEvent c = do
  case c of
    SelUp -> addSel (-1)
    SelDown -> addSel 1
    Deselect -> selectedTask .= Nothing
    DeleteSelected -> do
      use selectedTask >>= maybe (pure ()) (handleTaskEvent . DeleteAt)
    CompleteSelected ->
      use selectedTask >>= \case
        Nothing -> pure ()
        Just i -> do
          t <- use tasks <&> (`Seq.index` i)
          tasks %= Seq.deleteAt i
          now <- liftIO getZonedTime
          let newT = (timeFinished .~ Just now) t
          tasks %= (|> newT)
    Add p -> do
      tasks %= (p <|)
    Append p -> do
      tasks %= (|> p)
    SelectWithOld old -> do
      f <- use sendTask
      idx <- use selectedTask
      case idx of
        Just i -> do
          t <- (`Seq.index` i) <$> use tasks
          liftIO (f t)
          tasks %= Seq.deleteAt i
          maybe (pure ()) (\p -> tasks %= (p <|)) old
        Nothing -> pure ()
    Save -> pure ()
    Load ts -> do
      today <- zonedTimeToLocalDay <$> liftIO getZonedTime
      let finishedDay = preview $ timeFinished . _Just . to zonedTimeToLocalDay
      let filtered = filter (maybe True (== today) . finishedDay) ts
      liftIO (hPutStrLn stderr ("ts : " <> show ts <> "filtered: " <> show filtered))
      tasks .= (Seq.fromList filtered)
    DeleteAt i -> tasks %= Seq.deleteAt i
    InsertAt i t -> tasks %= Seq.insertAt i t
    SelectIndex i -> do
      l <- Seq.length <$> use tasks
      when (i >= 0 && i < l) (selectedTask .= Just i)
  -- NOTE: not needed for many of these cases but it's a cheap operation and a nice fail safe to have
  ts <- use tasks
  f <- use saveTasks'
  liftIO $ f (toList ts)
 where
  addSel :: Int -> EventM n TaskState ()
  addSel i = do
    l <- Seq.length <$> use tasks
    when (l > 0) $ do
      cur <- use selectedTask
      selectedTask .= case cur of
        Nothing -> Just 0
        Just n -> Just ((n + i) `mod` l)

colourFocused :: Bool -> Color
colourFocused focused = if focused then yellow else white

tasksW :: TaskState -> Widget n
tasksW ts =
  let taskWidgetList = toList $
        flip Seq.mapWithIndex (ts ^. tasks) $
          \i t ->
            padRight Max $
              if ts ^. selectedTask == Just i
                then withAttr (attrName "accent") (txt (taskPretty t))
                else txt (taskPretty t)
   in if null taskWidgetList
        then txt "no tasks ([i] to insert task)"
        else
          bord (colourFocused $ isJust (ts ^. selectedTask)) "Tasks" $
            vBox taskWidgetList

optionalEditField :: (Ord n, Show n, Show a, Read a) => Lens' s (Maybe a) -> n -> s -> FormFieldState s e n
optionalEditField lens name =
  let validateWrapper ls =
        let t = Text.strip (Text.intercalate "\n" ls)
         in if Text.null t
              then Just Nothing
              else Just <$> readMaybe (Text.unpack t)
      showV = \case
        Nothing -> ""
        Just a -> tShow a
   in editField lens name (Just 1) showV validateWrapper (txt . Text.unlines) id

mkTaskForm :: Task -> Form Task PomoEvent PomoResource
mkTaskForm =
  let label s w =
        (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
   in newForm
        [ label "Title:" @@= editTextField title TaskTitleField (Just 1)
        , label "Target cycles:" @@= optionalEditField target TaskTargetField
        ]

newTaskForm :: IO (Form Task PomoEvent PomoResource)
newTaskForm = do
  t <- getZonedTime
  pure $ mkTaskForm (Task "" 0 Nothing t Nothing)
