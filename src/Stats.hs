{-# LANGUAGE OverloadedStrings #-}

module Stats where

import Brick (EventM, Padding (Max), Widget, txt)
import Brick.Types (put)
import Brick.Widgets.Core (padRight, vBox)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import Data.Time (Day, NominalDiffTime, ZonedTime (zonedTimeToLocalTime), addLocalTime, addUTCTime, getCurrentTime, nominalDay, nominalDiffTimeToSeconds, utctDay, zonedTimeToUTC)
import Data.Time.Clock (UTCTime (..), diffUTCTime)
import Data.Time.LocalTime (getZonedTime, localDay)
import GHC.Generics (Generic)
import GHC.IO.StdHandles (stderr)
import Graphics.Vty.Attributes (brightWhite)
import Lens.Micro (Traversal', foldMapOf, (^.))
import Lens.Micro.Mtl ((.=))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Type (Getting)
import System.Process.Internals (ProcRetHandles (hStdError))
import Util

data DayStats = DayStats {_workTime :: NominalDiffTime}
  deriving (Eq, Show, Generic)

makeLenses ''DayStats

data StatsState = StatsState {_days :: Map Day DayStats}
  deriving (Eq, Show, Generic)

makeLenses ''StatsState

_LWorkedTime :: Traversal' Activity NominalDiffTime
_LWorkedTime f (LWorked desc t) = LWorked desc <$> f t
_LWorkedTime _ completed = pure completed

sumOf :: (Num a) => Getting (Sum a) s a -> s -> a
sumOf l = getSum . foldMapOf l Sum

workedTime :: [Activity] -> NominalDiffTime
workedTime = sumOf (traverse . _LWorkedTime)

priorWeek :: IO [Day]
priorWeek = do
  t <- zonedTimeToLocalTime <$> getZonedTime
  let foo i = negate (fromIntegral i * nominalDay)
  pure [localDay $ addLocalTime (foo i) t | i <- [0 :: Int .. 6]]

getStats :: IO StatsState
getStats = do
  week <- priorWeek
  foo <- traverse dayLogs =<< priorWeek
  pure . StatsState $ Map.fromList $ zip week $ map (DayStats . workedTime . (map activity)) foo

refreshState :: EventM PomoResource StatsState ()
refreshState = put =<< liftIO getStats

statPretty :: Day -> DayStats -> Text
statPretty day s =
  let (h :: Int, sLeft) = floor (nominalDiffTimeToSeconds (s ^. workTime)) `divMod` (60 * 60)
   in tShow day <> " - " <> tShow h <> "h" <> tShow (sLeft `div` 60) <> "m"

statsW :: StatsState -> Widget PomoResource
statsW s =
  bord brightWhite "stats" $
    vBox $
      map (padRight Max . txt . uncurry statPretty) (Map.toList (s ^. days))
