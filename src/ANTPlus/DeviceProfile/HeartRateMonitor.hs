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
  HeartRateDevice {heartRateDeviceChannel :: Channel}

data HeartRatePage =
  HeartRatePage {hrmPage :: PageDetails
                ,hrmHeartBeatEventTime :: Word64
                ,hrmHeartBeatCount :: Word8
                ,hrmComputedHeartRate :: Word8}
  deriving (Show)

data PageDetails
  = PageZero
  | PageOne Word64
  | PageTwo Word8 Word16
  | PageThree Word8 Word8 Word8
  | PageFour Word8 Word16
  | PageUnknown
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
                            PageOne (fromIntegral a .|.
                                     shiftL (fromIntegral b) 8 .|.
                                     shiftL (fromIntegral c) 16)
                          2 -> PageTwo a (fromIntegral b .|. shiftL (fromIntegral c) 8)
                          3 -> PageThree a b c
                          4 -> PageFour a (fromIntegral b .|. shiftL (fromIntegral c) 8)
                          _ -> PageUnknown)
                       (fromIntegral eventTimeLsb .|.
                        shiftL (fromIntegral eventTimeMsb) 8)
                       beatCount
                       heartRate))
       _ ->
         do putStrLn ("Unknown HRM packet: " ++ show bytes)
            return Nothing
