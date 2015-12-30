{-# LANGUAGE RecordWildCards #-}

module ANTPlus.Network where

import Control.Applicative
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Unique
import Data.Vector (Vector)
import Data.Word
import Numeric.Natural
import qualified ANTPlus
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified System.USB as USB

data ANTUSBDevice =
  ANTUSBDevice USB.Device

data ANT =
  ANT {antDh :: USB.DeviceHandle
      ,antOut :: USB.EndpointAddress
      ,antNumNetworks :: MVar Natural
      ,antReaders :: TVar (Map.Map Unique (ANTPlus.AntMessage -> STM ()))}

withReader :: ANT
           -> (ANTPlus.AntMessage -> Maybe a)
           -> (TBQueue a -> IO b)
           -> IO b
withReader ANT{..} condition m =
  do responses <- newTBQueueIO 1
     identifier <- newUnique
     let reader msg =
           case condition msg of
             Just msg' -> writeTBQueue responses msg'
             _ -> retry
     atomically
       (modifyTVar antReaders
                   (Map.insert identifier reader))
     finally (m responses)
             (atomically
                (modifyTVar' antReaders
                             (Map.delete identifier)))

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
     USB.setDebug ctx USB.PrintDebug
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
  USB.withDeviceHandle antStick $
  \antDh ->
    do USB.resetDevice antDh
       antConfigDesc <- USB.getConfigDesc antStick 0
       let interface = V.head (V.head (USB.configInterfaces antConfigDesc))
           findEndpoint dir =
             V.head (V.filter (\epDesc ->
                                 USB.transferDirection (USB.endpointAddress epDesc) ==
                                 dir)
                              (USB.interfaceEndpoints interface))
           [usbIn,usbOut] = map findEndpoint [USB.In,USB.Out]
       USB.withDetachedKernelDriver antDh
                                    (USB.interfaceNumber interface) $
         USB.withClaimedInterface antDh
                                  (USB.interfaceNumber interface) $
         do networks <- newMVar 0
            readers <- newTVarIO Map.empty
            let ant = ANT antDh (USB.endpointAddress usbOut) networks readers
            send ant ANTPlus.ResetSystem
            readMessage antDh
                        (USB.endpointAddress usbIn)
            withAsync (forever (do (bytes,status) <-
                                     readMessage antDh
                                                 (USB.endpointAddress usbIn)
                                   case ANTPlus.decodeSerialMessage bytes of
                                     Left e -> putStrLn e
                                     Right (ANTPlus.SerialMessage _ m) ->
                                       atomically
                                         (do currentReaders <- readTVar readers
                                             foldr (\reader other ->
                                                      reader m <|> other)
                                                   (return ())
                                                   currentReaders)))
                      (const (m ant))

withNetwork :: ANT -> (Network -> IO a) -> IO a
withNetwork ant@ANT{..} m =
  do number <-
       modifyMVar antNumNetworks
                  (\n -> pure (n + 1,n))
     channelsVar <- newMVar 0
     let message =
           ANTPlus.antMessage
             (ANTPlus.SetNetworkKeyPayload (ANTPlus.NetworkNumber (fromIntegral number))
                                           (ANTPlus.NetworkKey 185 165 33 251 189 114 195 69))
     response <-
       sendAwaitingResponse
         ant
         message
         (\response ->
            case response of
              ANTPlus.ChannelResponse payload
                | ANTPlus.channelResponseMessageId payload ==
                    ANTPlus.antMessageId message -> Just payload
                | otherwise -> Nothing)
     m (Network number channelsVar ant)

sendAwaitingResponse :: ANT
                     -> ANTPlus.AntMessage
                     -> (ANTPlus.AntMessage -> Maybe a)
                     -> IO a
sendAwaitingResponse ant@ANT{..} msg f =
  withReader
    ant
    f
    (\responses ->
       do send ant msg
          atomically (readTBQueue responses))

send
  :: ANT -> ANTPlus.AntMessage -> IO (USB.Size,USB.Status)
send ANT{..} msg =
  USB.writeBulk
    antDh
    antOut
    (LBS.toStrict
       (Build.toLazyByteString (ANTPlus.encodeSerialMessage (ANTPlus.SerialMessage ANTPlus.sync msg))))
    0

-- waitForResponse :: ANT -> (ANTPlus.ChannelResponsePayload -> Bool) -> IO ANTPlus.ChannelResponsePayload
-- waitForResponse ant f =
--   waitForMessage
--     ant
--     (\msg ->
--        case msg of
--          ANTPlus.ChannelResponse payload
--            | f payload -> Just payload
--          _ -> Nothing)

findAndRemove :: (a -> Maybe b) -> [a] -> (Maybe b, [a])
findAndRemove f [] = (Nothing, [])
findAndRemove f (x:xs) =
  case f x of
    Just b -> (Just b, xs)
    _ ->
      let (res,xs') = findAndRemove f xs
      in (res,x : xs')

withChannel
  :: Network -> Word8 -> (Channel -> IO a) -> IO a
withChannel Network{..} channelType m =
  do number <-
       modifyMVar networkNumChannels
                  (\n -> pure (n + 1,n))
     let channelNumber = ANTPlus.ChannelNumber (fromIntegral number)
         message =
           ANTPlus.antMessage
             (ANTPlus.AssignChannelPayload channelNumber
                                           (ANTPlus.ChannelType channelType)
                                           (ANTPlus.NetworkNumber (fromIntegral networkNumber)))
     sendAwaitingResponse
       networkAnt
       message
       (\response ->
          case response of
            ANTPlus.ChannelResponse payload
              | ANTPlus.channelResponseMessageId payload ==
                  ANTPlus.antMessageId message &&
                  ANTPlus.channelResponseChannelNumber payload == channelNumber ->
                Just payload
              | otherwise -> Nothing)
     m (Channel networkAnt channelNumber)

setChannelId :: Channel
             -> ANTPlus.DeviceNumber
             -> Bool
             -> Word8
             -> Word8
             -> IO ()
setChannelId Channel{..} deviceNumber pairing deviceType transmissionType =
  do let message =
           ANTPlus.antMessage
             (ANTPlus.SetChannelIdPayload channelNumber
                                          deviceNumber
                                          pairing
                                          deviceType
                                          transmissionType)
     sendAwaitingResponse
       channelANT
       message
       (\response ->
          case response of
            ANTPlus.ChannelResponse payload
              | ANTPlus.channelResponseMessageId payload ==
                  ANTPlus.antMessageId message &&
                  ANTPlus.channelResponseChannelNumber payload ==
                  channelNumber -> Just payload
              | otherwise -> Nothing)
     return ()

setChannelRFFreq :: Channel -> Word8 -> IO ()
setChannelRFFreq Channel{..} rfFreq =
  do let message =
           ANTPlus.antMessage (ANTPlus.SetChannelRFFreqPayload channelNumber rfFreq)
     sendAwaitingResponse
       channelANT
       message
       (\response ->
          case response of
            ANTPlus.ChannelResponse payload
              | ANTPlus.channelResponseMessageId payload ==
                  ANTPlus.antMessageId message &&
                  ANTPlus.channelResponseChannelNumber payload == channelNumber ->
                Just payload
              | otherwise -> Nothing)
     return ()

setChannelPeriod :: Channel -> Word16 -> IO ()
setChannelPeriod Channel{..} period =
  do let message =
           ANTPlus.antMessage (ANTPlus.SetChannelPeriodPayload channelNumber period)
     sendAwaitingResponse
       channelANT
       message
       (\response ->
          case response of
            ANTPlus.ChannelResponse payload
              | ANTPlus.channelResponseMessageId payload ==
                  ANTPlus.antMessageId message &&
                  ANTPlus.channelResponseChannelNumber payload == channelNumber ->
                Just payload
              | otherwise -> Nothing)
     return ()

openChannel :: Channel -> IO ()
openChannel Channel{..} =
  do let message =
           ANTPlus.antMessage (ANTPlus.OpenChannelPayload channelNumber)
     sendAwaitingResponse
       channelANT
       message
       (\response ->
          case response of
            ANTPlus.ChannelResponse payload
              | ANTPlus.channelResponseMessageId payload ==
                  ANTPlus.antMessageId message &&
                  ANTPlus.channelResponseChannelNumber payload == channelNumber ->
                Just payload
              | otherwise -> Nothing)
     return ()

readMessage :: USB.DeviceHandle -> USB.EndpointAddress -> IO (BS.ByteString, USB.Status)
readMessage dh ep =
  USB.readBulk dh ep 255 0
