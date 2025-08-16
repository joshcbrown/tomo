{-# LANGUAGE OverloadedStrings #-}

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
import Control.Exception (try)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

data Task = Task
  { _title :: Text
  , _nCompleted :: Int
  , _target :: Int
  , _timeCreated :: UTCTime
  , _timeFinished :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic)

makeLenses ''Task

tShow :: (Show a) => a -> Text
tShow = Text.pack . show

taskPretty :: Task -> Text
taskPretty p = prefix <> p ^. title <> " (" <> tShow (p ^. nCompleted) <> "/" <> tShow (p ^. target) <> ")"
 where
  prefix = if isJust (p ^. timeFinished) then "× " else "· "

instance ToJSON Task
instance FromJSON Task

data TimerEvent = TimeLeft NominalDiffTime | Done | SelectTask Task | Complete

data PomoEvent
  = TimerEvent TimerEvent
  | SaveTasks [Task]
  | CompleteTask Task

data PomoResource = TaskTitleField | TaskTargetField
  deriving (Eq, Ord, Show)

data AppFocus = Tasks | TaskForm | Unfocused

time :: NominalDiffTime -> NominalDiffTime -> (TimerEvent -> IO ()) -> IO ()
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

-- serialise/deserialise

appName :: String
appName = "pomos"

appDir :: IO FilePath
appDir = do
  home <- getHomeDirectory
  let dir = home </> "Library" </> "Application Support" </> appName
  createDirectoryIfMissing True dir
  pure dir

tasksFilePath :: IO FilePath
tasksFilePath = appDir <&> (</> "tasks.json")

saveTasks :: [Task] -> IO (())
saveTasks tasks = do
  try (tasksFilePath >>= \f -> LBS.writeFile f (encode tasks))
    >>= \case
      -- TODO: logging
      Left (_ :: IOError) -> pure ()
      Right _ -> pure ()

loadTasks :: IO [Task]
loadTasks = do
  result :: Either IOError (Maybe [Task]) <- try $ do
    filePath <- tasksFilePath
    content <- LBS.readFile filePath
    pure (decode content)
  pure $ fromMaybe [] $ fromRight Nothing result
