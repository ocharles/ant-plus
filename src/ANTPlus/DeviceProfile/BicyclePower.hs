{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module ANTPlus.DeviceProfile.BicyclePower where

import ANTPlus.Network
import Control.Monad
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Data.Bits
import Data.Word
import qualified ANTPlus
import qualified Data.ByteString as BS

data BicyclePower =
  BicyclePower {bicyclePowerPages :: !(TChan BicyclePowerPage)}

data BicyclePowerPage
  = BicyclePowerOnlyPage PowerOnlyPage
  | BicycleWheelTorquePage WheelTorquePage
  | BicycleCrankTorquePage CrankTorquePage
  | BicycleTorqueEffectivenessPage TorqueEffectivenessPage
  | BicycleCrankTorqueFrequency CrankTorqueFrequencyPage
  deriving (Show)

data PowerOnlyPage =
  PowerOnlyPage {powerOnlyPageUpdateEventCount :: !Word8
                ,powerOnlyPagePedalPower :: !Word8
                ,powerOnlyPageInstantenousCadance :: !Word8
                ,powerOnlyPageAccumulatedPower :: !Word16
                ,powreOnlyPageInstantaneousPower :: !Word16}
  deriving (Show)

data WheelTorquePage =
  WheelTorquePage {wheelTorquePageUpdateEventCount :: !Word8
                  ,wheelTorquePageWheelTicks :: !Word8
                  ,wheelTorquePageInstantenousCadance :: !Word8
                  ,wheelTorquePageWheelPeriod :: !Word16
                  ,wheelTorquePageAccumulatedTorque :: !Word16}
  deriving (Show)

data CrankTorquePage =
  CrankTorquePage {crankTorquePageUpdateEventCount :: !Word8
                  ,crankTorquePageCrankTicks :: !Word8
                  ,crankTorquePageInstantenousCadance :: !Word8
                  ,crankTorquePagePeriod :: !Word16
                  ,crankTorquePageAccumulatedTorque :: !Word16}
  deriving (Show)

data TorqueEffectivenessPage =
  TorqueEffectivenessPage {torqueEffectivenessPageUpdateEventCount :: !Word8
                          ,torqueEffectivenessLeftTorqueEffectiveness :: !Word8
                          ,torqueEffectivenessRightTorqueEffectiveness :: !Word8
                          ,torqueEffectivenessLeftPedalSmoothness :: !Word8
                          ,torqueEffectivenessRightPedalSmoothness :: !Word8
                          ,torqueEffectivenessPageReserved :: !(Word8,Word8)}
  deriving (Show)

data CrankTorqueFrequencyPage =
  CrankTorqueFrequencyPage {crankTorqueFrequencyPageUpdateEventCount :: !Word8
                          ,crankTorqueFrequencySlope :: !Word16
                          ,crankTorqueFrequencyTimeStamp :: !Word16
                          ,crankTorqueFrequencyTorqueTicksStamp :: !Word16}
  deriving (Show)

pattern DEVICE_TYPE_BICYCLE_POWER = 11

withBicyclePower
  :: Network -> (BicyclePower -> IO a) -> IO a
withBicyclePower network m =
  withChannel
    network
    0
    (\c ->
       do setChannelId c
                       (ANTPlus.DeviceNumber 0) -- 25864
                       False
                       DEVICE_TYPE_BICYCLE_POWER
                       0
          setChannelPeriod c 8182
          setChannelRFFreq c 57
          openChannel c
          pages <- newBroadcastTChanIO
          withReader
            (networkAnt network)
            (isHRMMessage c)
            (\hrmMsgs ->
               withAsync (forever (do bytes <- atomically (readTBQueue hrmMsgs)
                                      case parsePage bytes of
                                        Just page ->
                                          atomically (writeTChan pages page)
                                        Nothing ->
                                          putStrLn ("Unknown bicycle power packet: " ++
                                                    show bytes)))
                         (\_ -> m (BicyclePower pages))))
  where isHRMMessage Channel{..} =
          \case
            ANTPlus.BroadcastData ANTPlus.BroadcastDataPayload{..}
              | broadcastDataChannelNumber == channelNumber ->
                Just broadcastDataData
            ANTPlus.AcknowledgedData ANTPlus.AcknowledgedDataPayload{..}
              | acknowledgedDataChannelNumber == channelNumber ->
                Just acknowledgedDataData
            ANTPlus.BurstData ANTPlus.BurstDataPayload{..}
              | burstDataChannelNumber == channelNumber ->
                Just burstDataData
            _ -> Nothing

parsePage :: BS.ByteString -> Maybe BicyclePowerPage
parsePage bytes =
  case BS.unpack bytes of
    [16,a,b,c,d,e,f,g] ->
      Just (BicyclePowerOnlyPage
              (PowerOnlyPage a
                             b
                             c
                             (fromIntegral d .|. shiftL (fromIntegral e) 8)
                             (fromIntegral f .|. shiftL (fromIntegral g) 8)))
    [17,a,b,c,d,e,f,g] ->
      Just (BicycleWheelTorquePage
              (WheelTorquePage a
                               b
                               c
                               (fromIntegral d .|. shiftL (fromIntegral e) 8)
                               (fromIntegral f .|. shiftL (fromIntegral g) 8)))
    [18,a,b,c,d,e,f,g] ->
      Just (BicycleCrankTorquePage
              (CrankTorquePage a
                               b
                               c
                               (fromIntegral d .|. shiftL (fromIntegral e) 8)
                               (fromIntegral f .|. shiftL (fromIntegral g) 8)))
    [19,a,b,c,d,e,f,g] ->
      Just (BicycleTorqueEffectivenessPage
              (TorqueEffectivenessPage a
                                       b
                                       c
                                       d
                                       e
                                       (f,g)))
    [32,a,b,c,d,e,f,g] ->
      Just (BicycleCrankTorqueFrequency
              (CrankTorqueFrequencyPage
                 a
                 (fromIntegral b .|. shiftL (fromIntegral c) 8)
                 (fromIntegral d .|. shiftL (fromIntegral e) 8)
                 (fromIntegral f .|. shiftL (fromIntegral g) 8)))
    _ -> Nothing

newBicyclePowerPageReader :: BicyclePower -> IO (STM BicyclePowerPage)
newBicyclePowerPageReader (BicyclePower broadcast) = do
  pages <- atomically (dupTChan broadcast)
  pure (readTChan pages)
