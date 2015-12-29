{-# LANGUAGE RecordWildCards #-}

module ANTPlus.Network where

import Control.Concurrent.MVar
import Data.Vector (Vector)
import Data.Word
import Numeric.Natural
import qualified ANTPlus
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import qualified System.USB as USB

data ANTUSBDevice =
  ANTUSBDevice USB.Device

data ANT =
  ANT {antDh :: USB.DeviceHandle
      ,antIn :: USB.EndpointAddress
      ,antOut :: USB.EndpointAddress
      ,antNumNetworks :: MVar Natural}

data Network =
  Network {networkNumber :: Natural
          ,networkNumChannels :: MVar Natural
          ,networkAnt :: ANT}

data Channel =
  Channel {channelANT :: ANT
          ,channelNumber :: ANTPlus.ChannelNumber}

findANTUSBDevices :: IO (Vector ANTUSBDevice)
findANTUSBDevices =
  do ctx <- USB.newCtx
     devices <- USB.getDevices ctx
     fmap (fmap ANTUSBDevice)
          (V.filterM isAntStick devices)
  where isAntStick device =
          do desc <- USB.getDeviceDesc device
             pure (USB.deviceVendorId desc == 4047 &&
                   USB.deviceProductId desc == 4104)

withANTUSBDevice
  :: ANTUSBDevice -> (ANT -> IO a) -> IO a
withANTUSBDevice (ANTUSBDevice antStick) m =
  USB.withDeviceHandle
    antStick
    (\antDh ->
       do antConfigDesc <-
            USB.getConfigDesc antStick 0
          let interface =
                V.head (V.head (USB.configInterfaces antConfigDesc))
              findEndpoint dir =
                V.head (V.filter (\epDesc ->
                                    USB.transferDirection (USB.endpointAddress epDesc) ==
                                    dir)
                                 (USB.interfaceEndpoints interface))
              [usbIn,usbOut] =
                map findEndpoint [USB.In,USB.Out]
          USB.withDetachedKernelDriver
            antDh
            (USB.interfaceNumber interface)
            (USB.withClaimedInterface
               antDh
               (USB.interfaceNumber interface)
               (do networks <- newMVar 0
                   let ant =
                         ANT antDh
                             (USB.endpointAddress usbIn)
                             (USB.endpointAddress usbOut)
                             networks
                   send ant ANTPlus.ResetSystem
                   readMessage ant
                   m ant)))

withNetwork :: ANT -> (Network -> IO a) -> IO a
withNetwork ant@ANT{..} m =
  do number <-
       modifyMVar antNumNetworks
                  (\n -> pure (n + 1,n))
     channelsVar <- newMVar 0
     send ant
          (ANTPlus.antMessage
             (ANTPlus.SetNetworkKeyPayload (ANTPlus.NetworkNumber (fromIntegral number))
                                           (ANTPlus.NetworkKey 185 165 33 251 189 114 195 69)))
     readMessage ant
     m (Network number channelsVar ant)

withChannel
  :: Network -> Word8 -> (Channel -> IO a) -> IO a
withChannel Network{..} channelType m =
  do number <-
       modifyMVar networkNumChannels
                  (\n -> pure (n + 1,n))
     let channelNumber =
           ANTPlus.ChannelNumber (fromIntegral number)
     send networkAnt
          (ANTPlus.antMessage
             (ANTPlus.AssignChannelPayload channelNumber
                                           (ANTPlus.ChannelType channelType)
                                           (ANTPlus.NetworkNumber (fromIntegral networkNumber))))
     readMessage networkAnt
     m (Channel networkAnt channelNumber)

setChannelId :: Channel
             -> ANTPlus.DeviceNumber
             -> Bool
             -> Word8
             -> Word8
             -> IO ()
setChannelId Channel{..} deviceNumber pairing deviceType transmissionType =
  do send channelANT
          (ANTPlus.antMessage
             (ANTPlus.SetChannelIdPayload channelNumber
                                          deviceNumber
                                          pairing
                                          deviceType
                                          transmissionType))
     readMessage channelANT
     return ()

setChannelRFFreq :: Channel -> Word8 -> IO ()
setChannelRFFreq Channel{..} rfFreq =
  do send channelANT (ANTPlus.antMessage (ANTPlus.SetChannelRFFreqPayload channelNumber rfFreq))
     readMessage channelANT
     return ()

setChannelPeriod :: Channel -> Word16 -> IO ()
setChannelPeriod Channel{..} period =
  do send channelANT (ANTPlus.antMessage (ANTPlus.SetChannelPeriodPayload channelNumber period))
     readMessage channelANT
     return ()

openChannel :: Channel -> IO ()
openChannel Channel{..} =
  do send channelANT (ANTPlus.antMessage (ANTPlus.OpenChannelPayload channelNumber))
     return ()

send
  :: ANT -> ANTPlus.AntMessage -> IO (USB.Size,USB.Status)
send ANT{..} msg =
  USB.writeBulk
    antDh
    antOut
    (LBS.toStrict
       (Build.toLazyByteString (ANTPlus.encodeSerialMessage (ANTPlus.SerialMessage ANTPlus.sync msg))))
    0

readMessage :: ANT -> IO (BS.ByteString, USB.Status)
readMessage ANT{..} =
  USB.readBulk antDh antIn 255 0
