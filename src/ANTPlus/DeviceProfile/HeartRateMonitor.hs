{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module ANTPlus.DeviceProfile.HeartRateMonitor where

import ANTPlus.Network
import Data.Bits
import Data.Word
import qualified ANTPlus
import qualified Data.ByteString as BS

data HeartRateDevice =
  HeartRateDevice {heartRateDeviceChannel :: !Channel}

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

withHeartRateDevice
  :: Network -> (HeartRateDevice -> IO a) -> IO a
withHeartRateDevice network m =
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
          m (HeartRateDevice c))

pattern PAGE_TOGGLE = 0x80

awaitHeartRatePage :: HeartRateDevice -> IO (Maybe HeartRatePage)
awaitHeartRatePage (HeartRateDevice Channel{..}) =
  do bytes <-
       waitForMessage
         channelANT
         (\case
            ANTPlus.BroadcastData ANTPlus.BroadcastDataPayload{..}
              | broadcastDataChannelNumber == channelNumber ->
                Just broadcastDataData
            _ -> Nothing)
     case BS.unpack bytes of
       [pageNumber,a,b,c,eventTimeLsb,eventTimeMsb,beatCount,heartRate] ->
         pure (Just (HeartRatePage
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
                       heartRate))
       _ ->
         do putStrLn ("Unknown HRM packet: " ++ show bytes)
            return Nothing
