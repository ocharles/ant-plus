{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module ANTPlus.DeviceProfile.HeartRateMonitor where

import ANTPlus.Network
import Control.Monad
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Data.Bits
import Data.Word
import qualified ANTPlus
import qualified Data.ByteString as BS

data HeartRateMonitor =
  HeartRateMonitor {heartRatePages :: !(TChan HeartRatePage)}

data HeartRatePage =
  HeartRatePage {hrmPage :: !PageDetails
                ,hrmHeartBeatEventTime :: !Word64
                ,hrmHeartBeatCount :: !Word8
                ,hrmComputedHeartRate :: !Word8}
  deriving (Show)

data PageDetails
  = PageZero
  | PageOperatingTime !OperatingTime
  | PageIdentification !Identification
  | PageVersion !Version
  | PagePreviousHeartBeatTime !PreviousHeartBeatTime
  | PageUnknown
  deriving (Show)

data OperatingTime =
  OperatingTime {operatingTime :: !Word64}
  deriving (Show)

data Identification =
  Identification {identificationManufacturer :: !Word8
                 ,identificationSerialNumber :: !Word16}
  deriving (Show)

data Version =
  Version {versionHardware :: !Word8
          ,versionSoftware :: !Word8
          ,versionModel :: !Word8}
  deriving (Show)

data PreviousHeartBeatTime =
  PreviousHeartBeatTime {previousHeartBeatManufacturerSSpecific :: !Word8
                        ,previousHeartBeatTime :: !Word16}
  deriving (Show)

withHeartRateMonitor
  :: Network -> (HeartRateMonitor -> IO a) -> IO a
withHeartRateMonitor network m =
  withChannel
    network
    0
    (\c ->
       do setChannelId c
                       (ANTPlus.DeviceNumber 0)
                       False
                       120
                       0
          setChannelRFFreq c 57
          setChannelPeriod c 8070
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
                                          putStrLn ("Unknown HRM packet: " ++
                                                    show bytes)))
                         (\_ -> m (HeartRateMonitor pages))))
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

pattern PAGE_TOGGLE = 0x80

parsePage :: BS.ByteString -> Maybe HeartRatePage
parsePage bytes =
  case BS.unpack bytes of
    [pageNumber,a,b,c,eventTimeLsb,eventTimeMsb,beatCount,heartRate] ->
      Just (HeartRatePage
              (case pageNumber .&. complement PAGE_TOGGLE of
                 0 -> PageZero
                 1 ->
                   PageOperatingTime
                     (OperatingTime
                        (fromIntegral a .|. shiftL (fromIntegral b) 8 .|.
                         shiftL (fromIntegral c) 16))
                 2 ->
                   PageIdentification
                     (Identification
                        a
                        (fromIntegral b .|. shiftL (fromIntegral c) 8))
                 3 -> PageVersion (Version a b c)
                 4 ->
                   PagePreviousHeartBeatTime
                     (PreviousHeartBeatTime
                        a
                        (fromIntegral b .|. shiftL (fromIntegral c) 8))
                 _ -> PageUnknown)
              (fromIntegral eventTimeLsb .|.
               shiftL (fromIntegral eventTimeMsb) 8)
              beatCount
              heartRate)
    _ -> Nothing

newHeartRatePageReader :: HeartRateMonitor -> IO (STM HeartRatePage)
newHeartRatePageReader (HeartRateMonitor broadcast) = do
  pages <- atomically (dupTChan broadcast)
  pure (readTChan pages)
