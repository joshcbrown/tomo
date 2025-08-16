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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Graphics.Vty (blue, green, red)
import Graphics.Vty qualified as Vty
import Lens.Micro ((&), (+~), (.~), (^.))
import Lens.Micro.Mtl (use, (%=), (+=), (.=))
import Lens.Micro.TH (makeLenses)
import System.Process
import Util

data Work = Work | ShortBreak | LongBreak

data TimerData = YetToStart | Paused | Playing ThreadId

data PomoSession = PomoSession
  { _task :: Maybe Task
  , _ty :: Work
  , _pLength :: NominalDiffTime
  , _pCycle :: Int
  , _remaining :: NominalDiffTime
  , _timerThread :: TimerData
  , _complete :: Task -> IO ()
  }

makeLenses ''PomoSession

ex :: PomoSession
ex =
  PomoSession
    { _task =
        Nothing
    , _ty = Work
    , _pLength = workLength
    , _pCycle = 1
    , _remaining = workLength
    , _timerThread = YetToStart
    , _complete = \_ -> pure ()
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

toggleTimer :: BChan PomoEvent -> EventM n PomoSession ()
toggleTimer chan =
  use timerThread
    >>= \case
      Playing tid -> liftIO (killThread tid) *> (timerThread .= Paused)
      _ -> (startTimer chan)

startTimer :: BChan PomoEvent -> EventM n PomoSession ()
startTimer chan =
  use remaining >>= start
 where
  tick = secondsToNominalDiffTime 0.1
  start l =
    liftIO (forkIO (time tick l (writeBChan chan . TimerEvent))) >>= (timerThread .=) . Playing

skip :: BChan PomoEvent -> EventM n PomoSession ()
skip chan =
  use timerThread >>= \case
    Playing tid -> liftIO (killThread tid *> writeBChan chan (TimerEvent Done))
    _ -> liftIO (writeBChan chan (TimerEvent Done))

handle :: TimerEvent -> EventM n PomoSession ()
handle = \case
  TimeLeft d -> remaining .= d
  SelectTask p -> task .= Just p
  Done -> do
    timerThread .= YetToStart
    let workTrans = (pCycle += 1) *> pure (Work, workLength)
    (newTy, newLength) <-
      use ty >>= \case
        ShortBreak -> workTrans
        LongBreak -> workTrans
        Work -> do
          task %= fmap (nCompleted +~ 1)
          c <- use pCycle
          pure $
            if c `mod` 4 == 0
              then (LongBreak, longBreakLength)
              else (ShortBreak, shortBreakLength)
    ty .= newTy
    remaining .= newLength
    pLength .= newLength
    notifyMac
  Complete -> do
    now <- liftIO getCurrentTime
    use task >>= \case
      Just t -> use complete >>= \f -> liftIO (f $ t & timeFinished .~ Just now)
      Nothing -> pure ()
    task .= Nothing
 where
  notifyMac :: EventM n PomoSession ()
  notifyMac = do
    (message, soundName) <-
      use ty >>= \case
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

workPretty :: Work -> Text
workPretty = \case
  Work -> "Work"
  ShortBreak -> "Short break"
  LongBreak -> "Long break"

workColour :: Work -> Vty.Color
workColour = \case
  Work -> red
  ShortBreak -> green
  LongBreak -> blue

pomoW :: PomoSession -> Widget n
pomoW p =
  bord (workColour $ p ^. ty) (titleText <> titleSuffix) $
    vBox $
      maybe rest ((: rest) . workingOn) (p ^. task)
 where
  titleText = workPretty (p ^. ty)
  titleSuffix = case (p ^. timerThread) of
    Playing _ -> ""
    YetToStart -> " - yet to begin"
    Paused -> " - paused"
  workingOn pom = txt $ "Working on: " <> pom ^. title <> " (" <> tShow (pom ^. nCompleted) <> "/" <> tShow (pom ^. target) <> ")"
  rest =
    [ txt $ "Cycle: " <> tShow (p ^. pCycle)
    , txt $ "Remaining: " <> tShow minutes <> "m" <> tShow seconds <> "s"
    , progressBar Nothing (1 - (realToFrac $ secondsLeft / lengthSeconds))
    ]
  secondsLeft = nominalDiffTimeToSeconds (p ^. remaining)
  lengthSeconds = nominalDiffTimeToSeconds (p ^. pLength)
  (minutes :: Int, seconds) = floor secondsLeft `divMod` 60
