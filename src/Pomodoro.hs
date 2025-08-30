{-# LANGUAGE OverloadedStrings #-}

module Pomodoro where

import Brick (EventM, txt)
import Brick.BChan (BChan, writeBChan)
import Brick.Types (
  Widget,
 )
import Brick.Widgets.Core (vBox)
import Brick.Widgets.ProgressBar (progressBar)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Monad (join, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Time
import Graphics.Vty (blue, green, red)
import Graphics.Vty qualified as Vty
import Lens.Micro ((&), (+~), (.~), (^.), _Just)
import Lens.Micro.Mtl (preuse, use, (%=), (+=), (.=))
import Lens.Micro.TH (makeLenses)
import System.Process
import Util hiding (logActivity)
import Prelude hiding (cycle)

data CycleType = Work | ShortBreak | LongBreak

data TimerData = YetToStart | Paused | Playing ThreadId

data PomoSession = PomoSession
  { _focusedTask :: Maybe Task
  , _cycleType :: CycleType
  , _cycleDuration :: NominalDiffTime
  , _remainingCycleDuration :: NominalDiffTime
  , _timeRemainingAtPause :: NominalDiffTime
  , _cycle :: Int
  , _timerThread :: TimerData
  , _complete :: Task -> IO ()
  , _logActivity :: Activity -> EventM PomoResource PomoSession ()
  }

makeLenses ''PomoSession

defaultPomoSession :: PomoSession
defaultPomoSession =
  PomoSession
    { _focusedTask =
        Nothing
    , _cycleType = Work
    , _cycleDuration = workLength
    , _timeRemainingAtPause = workLength
    , _cycle = 1
    , _remainingCycleDuration = workLength
    , _timerThread = YetToStart
    , _complete = \_ -> pure ()
    , _logActivity = \_ -> pure ()
    }

isPlaying :: TimerData -> Bool
isPlaying = \case
  Playing _ -> True
  _ -> False

-- state management
workLength, shortBreakLength, longBreakLength :: NominalDiffTime
workLength = secondsToNominalDiffTime $ 25 * 60
shortBreakLength = secondsToNominalDiffTime $ 5 * 60
longBreakLength = secondsToNominalDiffTime $ 15 * 60

getTitle :: EventM n PomoSession (Maybe Text)
getTitle = preuse (focusedTask . _Just . title)

logActivity' :: Activity -> EventM PomoResource PomoSession ()
logActivity' a = use logActivity >>= \f -> f a

logWorked :: EventM PomoResource PomoSession ()
logWorked = do
  t <- (-) <$> use timeRemainingAtPause <*> use remainingCycleDuration
  use cycleType >>= \case
    Work -> getTitle >>= logActivity' . flip LWorked t
    _ -> pure ()

logCompleted :: EventM PomoResource PomoSession ()
logCompleted = getTitle >>= maybe (pure ()) (logActivity' . LCompleted)

toggleTimer :: BChan PomoEvent -> EventM PomoResource PomoSession ()
toggleTimer chan =
  use timerThread
    >>= \case
      Playing tid -> do
        logWorked
        (timeRemainingAtPause .=) =<< use remainingCycleDuration
        liftIO (killThread tid) *> (timerThread .= Paused)
      _ -> do
        startTimer chan

startTimer :: BChan PomoEvent -> EventM PomoResource PomoSession ()
startTimer chan =
  use remainingCycleDuration >>= start
 where
  tick = secondsToNominalDiffTime 0.25
  start l =
    liftIO (forkIO (time tick l (writeBChan chan . TimerEvent))) >>= (timerThread .=) . Playing

skip :: BChan PomoEvent -> EventM PomoResource PomoSession ()
skip chan =
  use timerThread >>= \case
    Playing tid -> liftIO (killThread tid *> writeBChan chan (TimerEvent Done))
    _ -> liftIO (writeBChan chan (TimerEvent Done))

handlePomoEvent :: TimerEvent -> EventM PomoResource PomoSession ()
handlePomoEvent = \case
  TimeLeft d -> remainingCycleDuration .= d
  SelectTask p -> focusedTask .= Just p
  Done -> nextCycle True
  Skip -> nextCycle False
  Complete -> do
    now <- liftIO getZonedTime
    logCompleted
    use focusedTask >>= \case
      Just t -> use complete >>= \f -> liftIO (f $ t & timeFinished .~ Just now)
      Nothing -> pure ()
    focusedTask .= Nothing
 where
  nextCycle :: Bool -> EventM PomoResource PomoSession ()
  nextCycle logRemaining = do
    when logRemaining $ logWorked
    timerThread .= YetToStart
    let workTrans = (cycle += 1) *> pure (Work, workLength)
    (newTy, newLength) <-
      use cycleType >>= \case
        ShortBreak -> workTrans
        LongBreak -> workTrans
        Work -> do
          focusedTask %= fmap (nCompleted +~ 1)
          c <- use cycle
          pure $
            if c `mod` 4 == 0
              then (LongBreak, longBreakLength)
              else (ShortBreak, shortBreakLength)
    cycleType .= newTy
    remainingCycleDuration .= newLength
    cycleDuration .= newLength
    timeRemainingAtPause .= newLength
    notifyMac

  notifyMac :: EventM n PomoSession ()
  notifyMac = do
    (message, soundName) <-
      use cycleType >>= \case
        ShortBreak -> pure ("time for short break", "Funk")
        LongBreak -> pure ("time for long break", "Funk")
        Work -> pure ("time to work", "Purr")
    _ <-
      liftIO . forkIO . callCommand $
        "osascript -e 'display notification \""
          <> message
          <> "\" with title \"pomo\" sound name \""
          <> soundName
          <> "\"'"
    pure ()

-- UI

cycleTypePretty :: CycleType -> Text
cycleTypePretty = \case
  Work -> "Work"
  ShortBreak -> "Short break"
  LongBreak -> "Long break"

cycleTypeColour :: CycleType -> Vty.Color
cycleTypeColour = \case
  Work -> red
  ShortBreak -> green
  LongBreak -> blue

pomoW :: PomoSession -> Widget n
pomoW p =
  bord (cycleTypeColour $ p ^. cycleType) (titleText <> titleSuffix) $
    vBox $
      maybe rest ((: rest) . workingOn) (p ^. focusedTask)
 where
  titleText = cycleTypePretty (p ^. cycleType)
  titleSuffix = case (p ^. timerThread) of
    Playing _ -> ""
    YetToStart -> " - yet to begin"
    Paused -> " - paused"
  workingOn task = txt $ taskDescPretty task
  rest =
    [ txt $ "Cycle: " <> tShow (p ^. cycle)
    , txt $ "Remaining: " <> tShow minutes <> "m" <> tShow seconds <> "s"
    , progressBar Nothing (1 - (realToFrac $ secondsLeft / lengthSeconds))
    ]
  secondsLeft = nominalDiffTimeToSeconds (p ^. remainingCycleDuration)
  lengthSeconds = nominalDiffTimeToSeconds (p ^. cycleDuration)
  (minutes :: Int, seconds) = floor secondsLeft `divMod` 60
