{-# LANGUAGE RecordWildCards #-}

module ANTPlus.DeviceProfile.HeartRateMonitor where

import ANTPlus.Network
import Data.Word
import qualified ANTPlus
import qualified Data.ByteString as BS

data HeartRateDevice =
  HeartRateDevice {heartRateDeviceChannel :: Channel}

data HeartRatePage =
  HeartRatePage {hrmPage :: Word8
                ,hrmHeartBeatEventTime :: Word64
                ,hrmHeartBeatCount :: Word8
                ,hrmComputedHeartRate :: Word8}

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

awaitHeartRatePage :: HeartRateDevice -> IO (Maybe HeartRatePage)
awaitHeartRatePage (HeartRateDevice Channel{..}) =
  do (bytes,status) <- readMessage channelANT
     case BS.unpack bytes of
       [164,9,78,0,_,_,_,_,_,_,_,heartRate,_] ->
         pure (Just (HeartRatePage undefined undefined undefined heartRate))
       _ -> return Nothing
