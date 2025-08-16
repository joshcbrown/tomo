module Util where

import Brick (Widget, attrName, bg, fill, txt, (<+>))
import Brick.AttrMap (applyAttrMappings)
import Brick.Util (fg)
import Brick.Widgets.Border (borderAttr, borderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Core (updateAttrMap, withAttr, withBorderStyle)
import Brick.Widgets.ProgressBar (progressCompleteAttr)
import Data.Text (Text)
import Graphics.Vty (Color)
import Lens.Micro.TH (makeLenses)

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)

data Task = Task
  { _title :: Text
  , _nCompleted :: Int
  , _target :: Int
  , _timeCreated :: UTCTime
  , _timeFinished :: Maybe UTCTime
  }

makeLenses ''Task

data PomoEvent = TimeLeft NominalDiffTime | Done | SelectTask Task
data PomoResource = TaskTitleField | TaskTargetField
  deriving (Eq, Ord, Show)

data AppFocus = Tasks | TaskForm | Unfocused

time :: NominalDiffTime -> NominalDiffTime -> (PomoEvent -> IO ()) -> IO ()
time tickDuration duration notify = getCurrentTime >>= go
 where
  go start = loop (0 :: Int)
   where
    endTime = addUTCTime duration start
    loop tickN = do
      let tickTime = addUTCTime (fromIntegral tickN * tickDuration) start
      let (expectedTime, event) =
            if tickTime < endTime
              then (tickTime, TimeLeft $ endTime `diffUTCTime` tickTime)
              else (endTime, Done)
      sleepTime <- (expectedTime `diffUTCTime`) <$> getCurrentTime
      when (sleepTime > 0) $ threadDelay $ round (sleepTime * 1e6)
      notify event
      case event of
        Done -> pure ()
        _ -> loop (tickN + 1)

bord :: Color -> Text -> Widget n -> Widget n
bord col title w =
  updateAttrMap updateM $
    withBorderStyle unicodeRounded $
      borderWithLabel titleW w
 where
  titleW = withAttr (attrName "highlight") $ txt title <+> fill ('─')
  updateM =
    applyAttrMappings
      [ (borderAttr, fg col)
      , (attrName "highlight", fg col)
      , (progressCompleteAttr, bg col)
      ]
