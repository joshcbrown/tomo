{-# LANGUAGE OverloadedStrings #-}

module Tasks where

import Brick
import Brick.BChan
import Brick.Forms
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq (..), (<|))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Graphics.Vty (Color)
import Graphics.Vty.Attributes (brightWhite, yellow)
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import Pomodoro
import Util

data Control = SelUp | SelDown | Deselect | SelectWithOld (Maybe Pomo) | Add Pomo

data TaskState = TaskState
  { _tasks :: Seq Pomo
  , _selected :: Maybe Int
  , _sendTask :: Pomo -> IO ()
  }

exTasks :: TaskState
exTasks =
  TaskState
    { _tasks = Seq.fromList []
    , _selected = Nothing
    , _sendTask = \_ -> pure ()
    }

makeLenses ''TaskState

handleTaskEvent :: Control -> EventM n TaskState ()
handleTaskEvent = \case
  SelUp -> addSel (-1)
  SelDown -> addSel 1
  Deselect -> selected .= Nothing
  Add p -> tasks %= (p <|)
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
col focused = if focused then yellow else brightWhite

tasksW :: TaskState -> Widget n
tasksW ts =
  let blah = toList $
        flip Seq.mapWithIndex (ts ^. tasks) $
          \i t ->
            padRight Max $
              if ts ^. selected == Just i
                then withAttr (attrName "accent") (txt (pomoPretty t))
                else txt (pomoPretty t)
   in if null blah
        then txt "no tasks ([i] to insert task)"
        else
          bord (col $ isJust (ts ^. selected)) "tasks" $
            vBox $
              blah

mkTaskForm :: Pomo -> Form Pomo PomoEvent PomoResource
mkTaskForm =
  let label s w =
        (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
   in newForm
        [ label "title" @@= editTextField title TaskTitleField (Just 1)
        , label "target" @@= editShowableField target TaskTargetField
        ]
defaultTaskForm :: Form Pomo PomoEvent PomoResource
defaultTaskForm = mkTaskForm $ Pomo "" 0 0
