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
import Control.Exception (SomeException (SomeException), try)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text qualified as Text
import Data.Time (Day, LocalTime (localDay), NominalDiffTime, UTCTime (utctDay), ZonedTime (zonedTimeToLocalTime), addUTCTime, diffUTCTime, getCurrentTime, getZonedTime, zonedTimeToUTC)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import GHC.Generics (Generic)
import GHC.IO.Exception (IOException (..))
import Lens.Micro ((^.))
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

data Task = Task
  { _title :: Text
  , _nCompleted :: Int
  , _target :: Int
  , _timeCreated :: ZonedTime
  , _timeFinished :: Maybe ZonedTime
  }
  deriving (Show, Generic)

makeLenses ''Task

data TimerEvent = TimeLeft NominalDiffTime | Done | SelectTask Task | Complete | Skip

data PomoEvent
  = TimerEvent TimerEvent
  | SaveTasks [Task]
  | CompleteTask Task
  | RefreshStats

data PomoResource = TaskTitleField | TaskTargetField | StatsWidget
  deriving (Eq, Ord, Show)

data AppFocus = Tasks | TaskForm | Unfocused

data Activity = LWorked (Maybe Text) NominalDiffTime | LCompleted Text
  deriving (Show, Eq, Generic)

data AppLog = AppLog ZonedTime Activity
  deriving (Show, Generic)

activity :: AppLog -> Activity
activity (AppLog _ ac) = ac

instance ToJSON Activity
instance ToJSON AppLog
instance ToJSON Task
instance FromJSON Activity
instance FromJSON AppLog
instance FromJSON Task

tShow :: (Show a) => a -> Text
tShow = Text.pack . show

taskPretty :: Task -> Text
taskPretty p = prefix <> p ^. title <> " (" <> tShow (p ^. nCompleted) <> "/" <> tShow (p ^. target) <> ")"
 where
  prefix = if isJust (p ^. timeFinished) then "× " else "· "

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

ensureDir :: FilePath -> IO FilePath
ensureDir dir = createDirectoryIfMissing True dir *> pure dir

appDirPath :: IO FilePath
appDirPath = do
  home <- getHomeDirectory
  pure $ home </> "Library" </> "Application Support" </> appName

appDir :: IO FilePath
appDir = appDirPath >>= ensureDir

logsDir :: IO FilePath
logsDir = (appDirPath <&> (</> "logs")) >>= ensureDir

logFile :: Day -> IO FilePath
logFile t = (</>) <$> logsDir <*> pure (show t <> ".jsonl")

currentLogFile :: IO FilePath
currentLogFile = logFile =<< (zonedTimeToLocalDay <$> getZonedTime)

tasksFilePath :: IO FilePath
tasksFilePath = appDir <&> (</> "tasks.json")

-- TODO: exception handling (?)
saveTasks :: [Task] -> IO (())
saveTasks tasks = tasksFilePath >>= \f -> LBS.writeFile f (encode tasks)

nullIfThrow :: IO [a] -> IO [a]
nullIfThrow act = fromRight [] <$> try @IOException act

loadTasks :: IO [Task]
loadTasks = nullIfThrow $ do
  filePath <- tasksFilePath
  content <- LBS.readFile filePath
  pure $ fromMaybe [] (decode content)

logActivity :: Activity -> IO ()
logActivity a = do
  t <- getZonedTime
  f <- currentLogFile
  let line = encode (AppLog t a) <> "\n"
  LBS.appendFile f line

dayLogs :: Day -> IO [AppLog]
dayLogs t = nullIfThrow $ do
  contents <- LBS.readFile =<< logFile t
  let contentL = filter (not . LBS8.null) (LBS8.lines contents)
  pure $ mapMaybe decode contentL

todayLogs :: IO [AppLog]
todayLogs = dayLogs =<< (zonedTimeToLocalDay <$> getZonedTime)

-- misc
zonedTimeToLocalDay :: ZonedTime -> Day
zonedTimeToLocalDay = localDay . zonedTimeToLocalTime
