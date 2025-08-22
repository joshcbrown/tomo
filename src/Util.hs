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
import Graphics.Vty (Color, yellow)
import Lens.Micro.TH (makeLenses)

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, decodeStrict, encode, encodeFile)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text qualified as Text
import Data.Time (Day, LocalTime (localDay), NominalDiffTime, ZonedTime (zonedTimeToLocalTime), addUTCTime, diffUTCTime, getCurrentTime, getZonedTime)
import GHC.Generics (Generic)
import GHC.IO.Exception (IOException (..))
import Lens.Micro ((^.))
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

data Task = Task
  { _title :: Text
  , _nCompleted :: Int
  , _target :: Maybe Int
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

data PomoResource = TaskTitleField | TaskTargetField | StatsWidget | DayWidget Day
  deriving (Eq, Ord, Show)

data AppFocus = Tasks | TaskForm | Pomo
  deriving (Eq, Ord, Show)

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

taskDescPretty :: Task -> Text
taskDescPretty t = t ^. title <> " (" <> tShow (t ^. nCompleted) <> "/" <> maybe "-" tShow (t ^. target) <> ")"

taskPretty :: Task -> Text
taskPretty t = prefix <> taskDescPretty t
 where
  prefix = if isJust (t ^. timeFinished) then "× " else "· "

time :: NominalDiffTime -> NominalDiffTime -> (TimerEvent -> IO ()) -> IO ()
time tickDuration duration notify = getCurrentTime >>= go
 where
  go start = loop (0 :: Int)
   where
    endTime = addUTCTime duration start
    loop tickN = do
      let tickTime = addUTCTime (fromIntegral tickN * tickDuration) start
      let (expectedTime, timeLeft) =
            if tickTime < endTime
              then (tickTime, endTime `diffUTCTime` tickTime)
              else (endTime, 0)
      sleepTime <- (expectedTime `diffUTCTime`) <$> getCurrentTime
      when (sleepTime > 0) $ threadDelay $ round (sleepTime * 1e6)
      notify (TimeLeft timeLeft)
      if timeLeft > 0 then loop (tickN + 1) else notify Done

bord :: Color -> Text -> Widget n -> Widget n
bord col bTitle w =
  updateAttrMap updateM $
    withBorderStyle unicodeRounded $
      borderWithLabel titleW w
 where
  titleW = withAttr (attrName "highlight") $ txt bTitle <+> fill ('─')
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
saveTasks tasks = tasksFilePath >>= flip encodeFile tasks

nullIfThrow :: IO [a] -> IO [a]
nullIfThrow act = fromRight [] <$> try @IOException act

loadTasks :: IO [Task]
loadTasks = nullIfThrow $ do
  filePath <- tasksFilePath
  content <- BS.readFile filePath
  pure $ fromMaybe [] (decodeStrict content)

logActivity :: Activity -> IO ()
logActivity a = do
  t <- getZonedTime
  f <- currentLogFile
  let line = encode (AppLog t a) <> "\n"
  LBS.appendFile f line

dayLogs :: Day -> IO [AppLog]
dayLogs t = nullIfThrow $ do
  contents <- BS.readFile =<< logFile t
  let contentL = filter (not . BS8.null) (BS8.lines contents)
  pure $ mapMaybe decodeStrict contentL

todayLogs :: IO [AppLog]
todayLogs = dayLogs =<< (zonedTimeToLocalDay <$> getZonedTime)

-- misc
zonedTimeToLocalDay :: ZonedTime -> Day
zonedTimeToLocalDay = localDay . zonedTimeToLocalTime

helpText :: AppFocus -> Text
helpText focus =
  Text.intercalate "\n" $
    [ "Welcome! Getting around in tomo is done nearly entirely with the keyboard."
    , "Key binds are context sensitive. There are broadly three contexts to be aware of:\n"
    , Text.intercalate "\n\n" $ case focus of
        Pomo -> [helpTextFor Pomo, helpTextFor Tasks, helpTextFor TaskForm]
        Tasks -> [helpTextFor Tasks, helpTextFor TaskForm, helpTextFor Pomo]
        TaskForm -> undefined
    , "\nAdditionally, you can click on any square in the stats area to view how long"
    , "you worked for that day."
    , "\nUse [?] to exit from this menu."
    ]
 where
  helpTextFor :: AppFocus -> Text
  helpTextFor focus' = Text.intercalate "\n" $ case focus' of
    Pomo ->
      [ "When the timer is focused (as when the app is first opened):"
      , "[q]uit the app"
      , "[p]ause the timer"
      , "[n]ext: skip the current timer"
      , "[t]asks: show/hide the task area"
      , "[s]tats: show/hide the stats area"
      , "[c]omplete task currently being worked on"
      , "[i]nsert a task"
      , "[j]/[k]: focus tasks area"
      ]
    Tasks ->
      [ "When tasks are focused:"
      , "[j]: move selection down"
      , "[k]: move selection up"
      , "[Esc]: move focus to pomo"
      , "[Enter]: work on selected task"
      , "[d]elete the selected task"
      , "[c]omplete selected task"
      , "[e]dit selected task"
      ]
    TaskForm ->
      [ "When editing a task:"
      , "[Enter]: add task to task list"
      , "[Esc]: abort"
      ]

helpW :: AppFocus -> Widget PomoResource
helpW = bord yellow "Help" . txt . helpText
