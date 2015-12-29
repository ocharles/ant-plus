module Main where

import ANTPlus
import Control.Monad
import Data.Monoid ((<>))
import System.USB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V

main :: IO ()
main =
  do ctx <- newCtx
     --setDebug ctx PrintDebug
     devices <- getDevices ctx
     antStick <- fmap V.head (V.filterM isAntStick devices)
     antDesc <- getDeviceDesc antStick
     withDeviceHandle
       antStick
       (\antDh ->
          do antConfigDesc <- getConfigDesc antStick 0
             let interface = V.head (V.head (configInterfaces antConfigDesc))
                 findEndpoint dir =
                   V.head (V.filter (\epDesc ->
                                       transferDirection (endpointAddress epDesc) ==
                                       dir)
                                    (interfaceEndpoints interface))
                 [usbIn,usbOut] = map findEndpoint [In,Out]
             withDetachedKernelDriver
               antDh
               (interfaceNumber interface)
               (withClaimedInterface
                  antDh
                  (interfaceNumber interface)
                  (do let read =
                            do (bytes,status) <-
                                 readBulk antDh (endpointAddress usbIn) 255 0
                               print (BS.unpack bytes)
                               print status
                          send msg =
                            writeBulk antDh
                                      (endpointAddress usbOut)
                                      (LBS.toStrict
                                         (Build.toLazyByteString
                                            (encodeSerialMessage (SerialMessage sync msg))))
                                      0
                      putStrLn "Opened USB interface"
                      -- Reset
                      putStrLn "Performing ANT+ reset"
                      send ResetSystem
                      read
                      -- Set network key
                      putStrLn "Setting network key"
                      send (antMessage
                              (SetNetworkKeyPayload
                                 (NetworkNumber 0)
                                 (185,165,33,251,189,114,195,69)))
                      read
                      -- Assign channel
                      putStrLn "Opening HRM channel"
                      send (antMessage
                              (AssignChannelPayload (ChannelNumber 0)
                                                    (ChannelType 0)
                                                    (NetworkNumber 0)))
                      read
                      -- Set id
                      send
                        (antMessage
                           (SetChannelIdPayload (ChannelNumber 0)
                                                (DeviceNumber 0)
                                                False
                                                120
                                                0))
                      read
                      -- Set rf frequency
                      send
                        (antMessage
                           (SetChannelRFFreqPayload (ChannelNumber 0)
                                                    57))
                      read
                      -- Set period
                      send
                        (antMessage
                           (SetChannelPeriodPayload (ChannelNumber 0)
                                                    8070))
                      read
                      -- Open channel
                      send (antMessage (OpenChannelPayload (ChannelNumber 0)))
                      putStrLn "Channel open"
                      forever (do (bytes,status) <-
                                    readBulk antDh (endpointAddress usbIn) 100 0
                                  case BS.unpack bytes of
                                    [164,9,78,0,_,_,_,_,_,_,_,heartRate,_] ->
                                      putStrLn ("Heart rate: " <>
                                                show heartRate)
                                    _ -> return ())
                      return ())))

isAntStick :: Device -> IO Bool
isAntStick device =
  do desc <- getDeviceDesc device
     pure (deviceVendorId desc == 4047 && deviceProductId desc == 4104)
