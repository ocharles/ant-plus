{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module ANTPlus
       (AntMessage(..), UnAssignChannelPayload(..),
        AssignChannelPayload(..), SetChannelIdPayload(..),
        SetChannelPeriodPayload(..), SetChannelRFFreqPayload(..),
        SetChannelSearchTimeoutPayload(..), SetChannelTxPowerPayload(..),
        SetLowPriorityChannelSearchTimeoutPayload(..),
        SetNetworkKeyPayload(..), SetSearchWaveformPayload(..),
        SetTransmitPowerPayload(..), SetSerialNumChannelIdPayload(..),
        OpenChannelPayload(..), SerialMessage(..), encodeSerialMessage,
        sync, antMessage, NetworkNumber(..), ChannelNumber(..),
        ChannelType(..), DeviceNumber(..))
       where

import Control.Monad
import Data.Bits (xor, clearBit, setBit)
import Data.Bool
import Data.Digest.CRC32
import Data.Foldable
import Data.Monoid ((<>))
import Data.Tagged
import Data.Text (Text)
import Data.Word (Word8, Word16)
import System.USB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V

--------------------------------------------------------------------------------
data AntMessage
  = UnAssignChannel UnAssignChannelPayload
  | AssignChannel AssignChannelPayload
  | SetChannelId SetChannelIdPayload
  | SetChannelPeriod SetChannelPeriodPayload
  | SetChannelRFFreq SetChannelRFFreqPayload
  | SetNetworkKey SetNetworkKeyPayload
  | SetTransmitPower SetTransmitPowerPayload
  | SetSearchWaveform SetSearchWaveformPayload
  -- | AddChannelID{} -> 0x59
  -- | AddCryptoID{} -> 0x59
  -- | ConfigList{} -> 0x5A
  -- | ConfigCryptoList{} -> 0x5A 
  | SetChannelTxPower SetChannelTxPowerPayload
  | SetLowPriorityChannelSearchTimeout SetLowPriorityChannelSearchTimeoutPayload
  | SetSerialNumChannelId SetSerialNumChannelIdPayload 
  -- | RxExtMesgsEnable{} -> 0x66
  -- | EnableLED{} -> 0x68
  -- | CrystalEnable{} -> 0x6D
  -- | LibConfig{} -> 0x6E
  -- | ConfigFrequencyAgility{} -> 0x70
  -- | SetProximitySearch{} -> 0x71
  -- | ConfigEventBuffer{} -> 0x74
  | SetChannelSearchTimeout SetChannelSearchTimeoutPayload
  -- | Set128BitNetworkKey{} -> 0x76
  -- | ConfigHighDutySearch{} -> 0x77
  -- | ConfigAdvancedBurst{} -> 0x78
  -- | ConfigEventFilter{} -> 0x79
  -- | ConfigSelectiveDataUpdate{} -> 0x7A
  -- | SetSduMask{} -> 0x7B
  -- | ConfigUserNVM{} -> 0x7C
  -- | EncryptedChannelEnable{} -> 0x7D
  -- | SetCryptoKey{} -> 0x7E
  -- | SetCryptoInfo{} -> 0x7F
  -- | SetSearchSharingCycles{} -> 0x81
  -- | CryptoKeyNVMOp{} -> 0x83
  -- | SetUSBDescriptorString{} -> 0xC7
  -- | StartUpMessage{} -> 0x6F
  -- | SerialErrorMessage{} -> 0xAE
  | ResetSystem
  | OpenChannel OpenChannelPayload

--------------------------------------------------------------------------------
class IsAntMessage a where
  antMessage :: a -> AntMessage

--------------------------------------------------------------------------------
data UnAssignChannelPayload =
  UnAssignChannelPayload {unassignChannelNumber :: !Word8}

instance IsAntMessage UnAssignChannelPayload where
  antMessage = UnAssignChannel

--------------------------------------------------------------------------------
newtype ChannelType = ChannelType Word8

pattern Receive = ChannelType 0x00
pattern Transmit = ChannelType 0x10
pattern TransmitOnly = ChannelType 0x50
pattern ReceiveOnly = ChannelType 0x40
pattern SharedReceive = ChannelType 0x20
pattern SharedTransmit = ChannelType 0x30

--------------------------------------------------------------------------------
newtype ChannelNumber = ChannelNumber Word8

--------------------------------------------------------------------------------
newtype NetworkNumber = NetworkNumber Word8

--------------------------------------------------------------------------------
data AssignChannelPayload =
  AssignChannelPayload {assignChannelChannelNumber :: !ChannelNumber
                       ,assignChannelType :: !ChannelType
                       ,assignChannelNetworkNumber :: !NetworkNumber}

instance IsAntMessage AssignChannelPayload where
  antMessage = AssignChannel

--------------------------------------------------------------------------------
data SetChannelIdPayload =
  SetChannelIdPayload {setChannelIdChannelNumber :: !ChannelNumber
                      ,setChannelIdDeviceNumber :: !DeviceNumber
                      ,setChannelIdDevicePairing :: Bool
                      ,setChannelIdDeviceType :: !Word8
                      ,setChannelIdTransmissionType :: !Word8}

instance IsAntMessage SetChannelIdPayload where
  antMessage = SetChannelId

--------------------------------------------------------------------------------
data SetChannelPeriodPayload =
  SetChannelPeriodPayload {setChannelPeriodChannelNumber :: !ChannelNumber
                          ,setChannelPeriodMessagingPeriod :: !Word16}

instance IsAntMessage SetChannelPeriodPayload where
  antMessage = SetChannelPeriod

--------------------------------------------------------------------------------
data SetChannelSearchTimeoutPayload =
  SetChannelSearchTimeoutPayload {setChannelSearchTimeoutChannelNumber :: !ChannelNumber
                                 ,setChannelSearchTimeoutSearchTimeout :: !Word8}

instance IsAntMessage SetChannelSearchTimeoutPayload where
  antMessage = SetChannelSearchTimeout

--------------------------------------------------------------------------------
data SetChannelRFFreqPayload =
  SetChannelRFFreqPayload {setChannelRFFreqChannelNumber :: !ChannelNumber
                          ,setChannelRFFreqRFFrequency :: !Word8}

instance IsAntMessage SetChannelRFFreqPayload where
  antMessage = SetChannelRFFreq

--------------------------------------------------------------------------------
data SetNetworkKeyPayload =
  SetNetworkKeyPayload {setNetworkKeyNetworkNumber :: !NetworkNumber
                       ,setNetworkKeyNetworkKey :: !(Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)}

instance IsAntMessage SetNetworkKeyPayload where
  antMessage = SetNetworkKey

--------------------------------------------------------------------------------
newtype PowerLevel = PowerLevel Word8

pattern PowerLevel0 = PowerLevel 0
pattern PowerLevel1 = PowerLevel 1
pattern PowerLevel2 = PowerLevel 2
pattern PowerLevel3 = PowerLevel 3
pattern PowerLevel4 = PowerLevel 4

data SetTransmitPowerPayload =
  SetTransmitPowerPayload {setTransmitPowerTransmitPower :: PowerLevel}

newtype SearchWaveform = SearchWaveform Word8

pattern StandardSearchWaveform = SearchWaveform 0
pattern FastSearchWaveform = SearchWaveform 1

data SetSearchWaveformPayload =
  SetSearchWaveformPayload {setSearchWaveformChannelNumber :: !Word8
                           ,setSearchWaveformSearchWaveform :: !SearchWaveform}

data SetChannelTxPowerPayload =
  SetChannelTxPowerPayload {setChannelTxPowerChannelNumber :: !Word8
                           ,setChannelTxPowerTransmitPower :: !PowerLevel}

data SetLowPriorityChannelSearchTimeoutPayload =
  SetLowPriorityChannelSearchTimeoutPayload {setLowPriorityChannelSearchTimeoutChannelNumber :: !Word8
                                            ,setLowPriorityChannelSearchTimeoutSearchTimeout :: !Word8}

data SetSerialNumChannelIdPayload =
  SetSerialNumChannelIdPayload {setSerialNumChannelIdChannelNumber :: !Word8
                               ,setSerialNumChannelIdPairingRequest :: !Bool
                               ,setSerialNumChannelIdDeviceTypeId :: !Word8
                               ,setSerialNumChannelIdTransmissionType :: !Word8}

data EnableExtendedMessagesPayload =
  EnableExtendedMessagesPayload {enableExtendedMessagesEnable :: !Bool}

data EnableLedPayload =
  EnableLedPayload {enableLedEnable :: !Bool}

data EnableCrystalPayload =
  EnableCrystalPayload {enableCrystalEnable :: !Bool}

data EnableLibConfigPayload =
  EnableLibConfigPayload {enableLibConfigEnableRxTimestamp :: !Bool
                         ,enableLibConfigEnableRSSI :: !Bool
                         ,enableLibConfigEnableChannelId :: !Bool}

data ConfigFrequencyAgilityPayload =
  ConfigFrequencyAgilityPayload {configFrequencyAgilityChannelNumber :: !Word8
                                ,configFrequencyAgilityUcFrequency1 :: !Word8
                                ,configFrequencyAgilityUcFrequency2 :: !Word8
                                ,configFrequencyAgilityUcFrequency3 :: !Word8}

newtype SearchThreshold = SearchThreshold Word8

pattern Threshold1 = SearchThreshold 1
pattern Threshold2 = SearchThreshold 2
pattern Threshold3 = SearchThreshold 3
pattern Threshold4 = SearchThreshold 4
pattern Threshold5 = SearchThreshold 5
pattern Threshold6 = SearchThreshold 6
pattern Threshold7 = SearchThreshold 7
pattern Threshold8 = SearchThreshold 8
pattern Threshold9 = SearchThreshold 9
pattern Threshold10 = SearchThreshold 10

data SetProximitySearchPayload =
  SetProximitySearchPayload {setProximitySearchPayloadChannelNumber :: !Word8
                            ,setProximitySearchPayloadUcSearchThreshold :: !(Maybe SearchThreshold)}

data SetChannelSearchPriorityPayload =
  SetChannelSearchPriorityPayload {setChannelSearchPriorityChannelNumber :: !Word8
                                  ,setChannelSearchPrioritySearchPriority :: !Word8}


data Set128BitNetworkKeyPayload =
  Set128BitNetworkKeyPayload {set128BitNetworkKeyNetworkNumber :: !Word8
                             ,set128BitNetworkKeyNetworkKey :: !(Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)}

newtype Sync = Sync Word8

newtype MessageId = MessageId Word8

-- instance AntMessage ResetSystem where
--   antMessageId = Tagged 0x4a
--   antEncodeMessage _ = BS.pack [0]

type NetworkKey = BS.ByteString

-- data SetNetworkKey =
--   SetNetworkKey {snkNetworkNumber :: !NetworkNumber
--                 ,snkNetworkKey :: !NetworkKey}

-- instance AntMessage SetNetworkKey where
--   antMessageId = Tagged 70
--   antEncodeMessage (SetNetworkKey (NetworkNumber nn) key) = BS.singleton nn <> key

data StartUpMessagePayload =
  StartUpMessagePayload {startUpMessagePowerOnReset :: !Bool
                        ,startUpMessageHardwareResetLine :: !Bool
                        ,startUpMessageWatchDogReset :: !Bool
                        ,startUpMessageCommandReset :: !Bool
                        ,startUpMessageSynchronousReset :: !Bool
                        ,startUpMessageSuspendReset :: !Bool}

--------------------------------------------------------------------------------
data OpenChannelPayload =
  OpenChannelPayload {openChannelChannelNumber :: !ChannelNumber}

instance IsAntMessage OpenChannelPayload where
  antMessage = OpenChannel

--------------------------------------------------------------------------------
data CloseChannelPayload =
  CloseChannelPayload {closeChannelChannelNumber :: !ChannelNumber}

data OpenRxScanModePayload =
  OpenRxScanModePayload {openRxScanModeAllowSynchronousPacketsOnly :: !Bool}

-- data OpenChannel =
--   OpenChannel {ocChannelNumber :: Word8}

data BroadcastDataPayload a =
  BroadcastDataPayload {broadcastDataChannelNumber :: !ChannelNumber
                       ,broadcastDataData :: a}

data AcknowledgedDataPayload a =
  AcknowledgedDataPayload {acknowledgedDataChannelNumber :: !ChannelNumber
                          ,acknowledgedDataData :: a}

newtype SequenceNumber = SequenceNumber Word8

data BurstDataPayload a =
  BurstDataPayload {burstDataSequenceNumber :: !SequenceNumber
                   ,burstDataChannelNumber :: !ChannelNumber
                   ,burstDataData :: a}

data AdvancedBurstDataPayload a =
  AdvancedBurstDataPayload {advancedBurstDataSequenceNumber :: !SequenceNumber
                           ,advancedBurstDataChannelNumber :: !ChannelNumber
                           ,advancedBurstDataData :: a}

newtype MessageCode = MessageCode Word8

data ChannelResponsePayload =
  ChannelResponsePayload {channelResponseChannelNumber :: !ChannelNumber
                         ,channelResponseMessageId :: !MessageId
                         ,channelResponseMessageCode :: !MessageCode}

newtype ChannelState = ChannelState Word8

data ChannelStatusPayload =
  ChannelStatusPayload {channelStatusChannelNumber :: !ChannelNumber
                       ,channelStatusChannelType :: !ChannelType
                       ,channelStatusNetworkNumber :: !NetworkNumber
                       ,channelStatusChannelState :: !ChannelState}

newtype DeviceNumber = DeviceNumber Word16

newtype DeviceTypeId = DeviceTypeId Word8

newtype TransmissionType = TransmissionType Word8

data ChannelIdPayload =
  ChannelIdPayload {channelIdChannelNumber :: !ChannelNumber
                   ,channelIdDeviceNumber :: !DeviceNumber
                   ,channelIdDeviceTypeId :: !DeviceTypeId
                   ,channelIdTransmissionType :: !TransmissionType}

data ANTVersionPayload =
  ANTVersionPayload {antVersionVersion :: !Text}

data CapabilitiesPayload =
  CApabilitiesPayload {capabilitiesMaxANTChannels :: !Word8
                      ,capabilitiesMaxNetworks :: !Word8
                      ,capabilitiesNoReceiveChannels :: !Bool
                      ,capabilitiesNoTransmitChannels :: !Bool
                      ,capabilitiesNoReceiveMessages :: !Bool
                      ,capabilitiesNoTransmitMessages :: !Bool
                      ,capabilitiesNoAckdMessages :: !Bool
                      ,capabilitiesNoBurstMessages :: !Bool
                      ,capabilitiesNetworkEnabled :: !Bool
                      ,capabilitiesSerialNumberEnabled :: !Bool
                      ,capabilitiesPerChannelTxPowerEnabled :: !Bool
                      ,capabilitiesLowPrioritySearchEnabled :: !Bool
                      ,capabilitiesScriptEnabled :: !Bool
                      ,capabilitiesSearchListEnabled :: !Bool
                      ,capabilitiesLedEnabled :: !Bool
                      ,capabilitiesExtMessageEnabled :: !Bool
                      ,capabilitiesScanModeEnabled :: !Bool
                      ,capabilitiesProxSearchEnabled :: !Bool
                      ,capabilitiesExtAssignEnabled :: !Bool
                      ,capabilitiesFsANTFSEnabled :: !Bool
                      ,capabilitiesFIT1Enabled :: !Bool
                      ,capabilitiesAdvancedBurstEnabled :: !Bool
                      ,capabilitiesEventBufferingEnabled :: !Bool
                      ,capabilitiesEventFilteringEnabled :: !Bool
                      ,capabilitiesHighDutySearchEnabled :: !Bool
                      ,capabilitiesSearchSharingEnabled :: !Bool
                      ,capabilitiesSelectiveDataUpdatesEnabled :: !Bool
                      ,capabilitiesEncryptedChannelEnabled :: !Bool
                      ,capabilitiesRFActiveNotificationEnabled :: !Bool}

data DeviceSerialNumberPayload =
  DeviceSerialNumberPayload {deviceSerialNumberSerialNumber :: !(Word8,Word8,Word8,Word8)}

-- instance AntMessage OpenChannel where
--   antMessageId = Tagged 0x4B
--   antEncodeMessage (OpenChannel cn) = BS.pack [cn] 

-- class AntMessage a where
--   antMessageId :: Tagged a Word8
--   antEncodeMessage :: a -> BS.ByteString

sync :: Sync
sync = Sync 164

data SerialMessage =
  SerialMessage {smSync :: !Sync
                ,smMessage:: !AntMessage}

buildChannelNumber :: ChannelNumber -> Build.Builder
buildChannelNumber (ChannelNumber n) = Build.word8 n

buildNetworkNumber :: NetworkNumber -> Build.Builder
buildNetworkNumber (NetworkNumber n) = Build.word8 n

buildDeviceNumber :: DeviceNumber -> Build.Builder
buildDeviceNumber (DeviceNumber n) = Build.word16LE n

antEncodeMessage :: AntMessage -> Build.Builder
antEncodeMessage =
  \case
    UnAssignChannel UnAssignChannelPayload{..} ->
      Build.word8 unassignChannelNumber
    AssignChannel AssignChannelPayload{..} ->
      buildChannelNumber assignChannelChannelNumber <>
      (case assignChannelType of
         ChannelType t -> Build.word8 t) <>
      buildNetworkNumber assignChannelNetworkNumber
    SetChannelId SetChannelIdPayload{..} ->
      buildChannelNumber setChannelIdChannelNumber <>
      buildDeviceNumber setChannelIdDeviceNumber <>
      Build.word8
        (bool (clearBit setChannelIdDeviceType 0)
              (setBit setChannelIdDeviceType 0)
              setChannelIdDevicePairing) <>
      Build.word8 setChannelIdTransmissionType
    SetChannelPeriod SetChannelPeriodPayload{..} ->
      buildChannelNumber setChannelPeriodChannelNumber <>
      Build.word16LE setChannelPeriodMessagingPeriod
    SetChannelRFFreq SetChannelRFFreqPayload{..} ->
      buildChannelNumber setChannelRFFreqChannelNumber <>
      Build.word8 setChannelRFFreqRFFrequency
    SetNetworkKey SetNetworkKeyPayload{..} ->
      buildNetworkNumber setNetworkKeyNetworkNumber <>
      (case setNetworkKeyNetworkKey of
         (a,b,c,d,e,f,g,h) -> mconcat (map Build.word8 [a,b,c,d,e,f,g,h]))
    ResetSystem -> Build.word8 0
    OpenChannel OpenChannelPayload{..} ->
      buildChannelNumber openChannelChannelNumber

antMessageId :: AntMessage -> Word8
antMessageId =
  \case UnAssignChannel{} -> 0x41
        AssignChannel{} -> 0x42
        SetChannelId{} -> 0x51
        SetChannelPeriod{} -> 0x43
        SetChannelSearchTimeout{} -> 0x44
        SetChannelRFFreq{} -> 0x45
        SetNetworkKey{} -> 0x46
        SetTransmitPower{} -> 0x47
        SetSearchWaveform{} -> 0x49
        -- AddChannelID{} -> 0x59
        -- AddCryptoID{} -> 0x59
        -- ConfigList{} -> 0x5A
        -- ConfigCryptoList{} -> 0x5A
        SetChannelTxPower{} -> 0x60
        SetLowPriorityChannelSearchTimeout{} -> 0x63
        SetSerialNumChannelId{} -> 0x65
        -- RxExtMesgsEnable{} -> 0x66
        -- EnableLED{} -> 0x68
        -- CrystalEnable{} -> 0x6D
        -- LibConfig{} -> 0x6E
        -- ConfigFrequencyAgility{} -> 0x70
        -- SetProximitySearch{} -> 0x71
        -- ConfigEventBuffer{} -> 0x74
        -- SetChannelSearchPriority{} -> 0x75
        -- Set128BitNetworkKey{} -> 0x76
        -- ConfigHighDutySearch{} -> 0x77
        -- ConfigAdvancedBurst{} -> 0x78
        -- ConfigEventFilter{} -> 0x79
        -- ConfigSelectiveDataUpdate{} -> 0x7A
        -- SetSduMask{} -> 0x7B
        -- ConfigUserNVM{} -> 0x7C
        -- EncryptedChannelEnable{} -> 0x7D
        -- SetCryptoKey{} -> 0x7E
        -- SetCryptoInfo{} -> 0x7F
        -- SetSearchSharingCycles{} -> 0x81
        -- CryptoKeyNVMOp{} -> 0x83
        -- SetUSBDescriptorString{} -> 0xC7
        -- StartUpMessage{} -> 0x6F
        -- SerialErrorMessage{} -> 0xAE
        ResetSystem{} -> 0x4A
        OpenChannel{} -> 0x4B

encodeSerialMessage
  :: SerialMessage -> Build.Builder
encodeSerialMessage (SerialMessage (Sync syncByte) message) =
  let messageBytes = Build.toLazyByteString (antEncodeMessage message)
      payload =
        Build.toLazyByteString
          (mconcat [Build.word8 syncByte
                   ,Build.word8 (fromIntegral (LBS.length messageBytes))
                   ,Build.word8 (antMessageId message)] <>
           Build.lazyByteString messageBytes)
  in Build.lazyByteString payload <> Build.word8 (LBS.foldl' xor 0 payload)
