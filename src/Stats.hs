{-# LANGUAGE OverloadedStrings #-}

module Stats where

import Brick (EventM, Widget, bg, clickable, hBox, txt)
import Brick.AttrMap (AttrName, attrName)
import Brick.Types (put)
import Brick.Util (fg)
import Brick.Widgets.Core (vBox, withAttr, (<=>))
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Data.Time (Day, NominalDiffTime, ZonedTime (zonedTimeToLocalTime), addLocalTime, nominalDay, nominalDiffTimeToSeconds, weekAllDays)
import Data.Time.Calendar (DayOfWeek (Sunday))
import Data.Time.LocalTime (getZonedTime, localDay)
import GHC.Generics (Generic)
import Graphics.Vty (Attr, Color (..))
import Graphics.Vty.Attributes (brightWhite, color240, yellow)
import Lens.Micro (Traversal', foldMapOf, (^.))
import Lens.Micro.Mtl ((.=))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Type (Getting)
import Util

data DayStats = DayStats {_workTime :: NominalDiffTime}
  deriving (Eq, Show, Generic)

makeLenses ''DayStats

data StatsState = StatsState {_days :: Map Day DayStats, _selectedDay :: Day, _weeks :: [[Day]]}
  deriving (Eq, Show, Generic)

makeLenses ''StatsState

data StateEvent = Refresh | SelectDay Day

_LWorkedTime :: Traversal' Activity NominalDiffTime
_LWorkedTime f (LWorked desc t) = LWorked desc <$> f t
_LWorkedTime _ completed = pure completed

sumOf :: (Num a) => Getting (Sum a) s a -> s -> a
sumOf l = getSum . foldMapOf l Sum

workedTime :: [Activity] -> NominalDiffTime
workedTime = sumOf (traverse . _LWorkedTime)

priorYearDays :: ZonedTime -> [Day]
priorYearDays t =
  let
    foo i = localDay $ addLocalTime (negate (fromIntegral @Int i * nominalDay)) (zonedTimeToLocalTime t)
   in
    [foo i | i <- [0 .. 372]]

lastYearWeeks :: ZonedTime -> [[Day]]
lastYearWeeks t =
  let
    today = zonedTimeToLocalDay t
    foo i = localDay $ addLocalTime (negate (fromIntegral @Int i * nominalDay * 7)) (zonedTimeToLocalTime t)
   in
    [weekAllDays Sunday (foo i) | i <- [52, 51 .. 1]] ++ [filter (<= today) (weekAllDays Sunday today)]

statsFromLogs :: [AppLog] -> DayStats
statsFromLogs = DayStats . workedTime . map activity

getStats :: IO StatsState
getStats = do
  now <- getZonedTime
  daysStats <- (traverse (\d -> (,) <$> pure d <*> (statsFromLogs <$> dayLogs d))) (priorYearDays now)
  pure $ StatsState (Map.fromList daysStats) (zonedTimeToLocalDay now) (lastYearWeeks now)

handleStatsEvent :: StateEvent -> EventM PomoResource StatsState ()
handleStatsEvent = \case
  Refresh -> put =<< liftIO getStats
  SelectDay d -> selectedDay .= d

statPretty :: Day -> DayStats -> Text
statPretty day s =
  let (h :: Int, sLeft) = floor (nominalDiffTimeToSeconds (s ^. workTime)) `divMod` (60 * 60)
   in tShow day <> " - " <> tShow h <> "h" <> tShow (sLeft `div` 60) <> "m"

statsW :: StatsState -> Widget PomoResource
statsW s =
  bord brightWhite "Stats" $
    (hBox $ map vBox $ daysTransformed)
      <=> selectSummary
 where
  maxDay :: NominalDiffTime
  maxDay = maximum $ (^. workTime) <$> (s ^. days)

  -- TODO: surely there is a lens-y way to do this
  daysTransformed :: [[Widget PomoResource]]
  daysTransformed = flip (map . map) (s ^. weeks) $
    \day -> case Map.lookup day (s ^. days) of
      Nothing -> txt "x"
      Just stat ->
        let getAttr = if (day == s ^. selectedDay) then contribAttrHighlight else contribAttr
            attr = getAttr . valueToLevel $ (stat ^. workTime) / maxDay
         in clickable (DayWidget day) $ withAttr attr (txt "■")

  selectSummary :: Widget PomoResource
  selectSummary =
    let day = s ^. selectedDay
     in txt $ case (Map.lookup day (s ^. days)) of
          Nothing -> tShow day <> " - not found"
          Just stat -> statPretty day stat

data Level = L0 | L1 | L2 | L3 | L4
  deriving (Eq, Show, Enum)

valueToLevel :: NominalDiffTime -> Level
valueToLevel val
  | val == 0.0 = L0
  | val <= 0.1 = L1
  | val <= 0.3 = L2
  | val <= 0.6 = L3
  | otherwise = L4

levelToColor :: Level -> Color
levelToColor = \case
  L0 -> c 21 27 34
  L1 -> c 2 58 22
  L2 -> c 25 108 46
  L3 -> c 46 160 67
  L4 -> c 86 211 100
 where
  c r g b = color240 @Int r g b

contribAttr :: Level -> AttrName
contribAttr level = attrName $ "contribution-" ++ show level

contribAttrHighlight :: Level -> AttrName
contribAttrHighlight level = attrName ("contribution-" ++ show level) <> attrName ("-highlight")

levelAttrMap :: [(AttrName, Attr)]
levelAttrMap =
  join
    [ [(contribAttr level, fg (levelToColor level)), (contribAttrHighlight level, bg yellow)]
    | level <- [L0, L1, L2, L3, L4]
    ]
