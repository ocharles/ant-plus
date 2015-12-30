{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module ANTPlus
       (AntMessage(..), UnAssignChannelPayload(..),
        AssignChannelPayload(..), SetChannelIdPayload(..),
        SetChannelPeriodPayload(..), SetChannelRFFreqPayload(..),
        SetChannelSearchTimeoutPayload(..), SetChannelTxPowerPayload(..),
        SetLowPriorityChannelSearchTimeoutPayload(..),
        SetNetworkKeyPayload(..), SetSearchWaveformPayload(..),
        SetTransmitPowerPayload(..), SetSerialNumChannelIdPayload(..),
        OpenChannelPayload(..), EnableLEDPayload(..), LibConfigPayload(..),
        ConfigFrequencyAgilityPayload(..), CrystalEnablePayload(..),
        SetProximitySearchPayload(..), SerialMessage(..),
        Set128BitNetworkKeyPayload(..), StartUpMessagePayload(..),
        CloseChannelPayload(..), SetChannelSearchPriorityPayload(..),
        BroadcastDataPayload(..), BurstDataPayload(..),
        AcknowledgedDataPayload(..), OpenRxScanModePayload(..),
        ChannelResponsePayload(..), AdvancedBurstDataPayload(..),
        ChannelIdPayload(..), ChannelStatusPayload(..),
        ANTVersionPayload(..), CapabilitiesPayload(..), DeviceSerialNumberPayload(..),
        encodeSerialMessage, decodeSerialMessage, sync, antMessage, NetworkNumber(..),
        ChannelNumber(..), ChannelType(..), DeviceNumber(..),
        NetworkKey(..), NetworkKey128(..), pattern Receive,
        pattern Transmit, pattern TransmitOnly, pattern ReceiveOnly,
        pattern SharedReceive, pattern SharedTransmit, pattern PowerLevel0,
        pattern PowerLevel1, pattern PowerLevel2, pattern PowerLevel3,
        pattern PowerLevel4, pattern StandardSearchWaveform,
        pattern FastSearchWaveform, pattern Threshold1, pattern Threshold2,
        pattern Threshold3, pattern Threshold4, pattern Threshold5,
        pattern Threshold6, pattern Threshold7, pattern Threshold8,
        pattern Threshold9, pattern Threshold10, SequenceNumber(..),
        ChannelState(..), DeviceTypeId(..), TransmissionType(..), antMessageId)
       where

import Control.Applicative
import Control.Monad
import Data.Bits (xor, bit, clearBit, setBit, testBit, (.|.), (.&.))
import Data.Bool
import Data.Foldable
import Data.Monoid ((<>))
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Text (Text)
import Data.Word (Word8, Word16)
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Lazy as LBS

--------------------------------------------------------------------------------
data AntMessage
  = UnAssignChannel UnAssignChannelPayload
  | AssignChannel AssignChannelPayload
  | SetChannelId SetChannelIdPayload
  | SetChannelPeriod SetChannelPeriodPayload
  | SetChannelSearchTimeout SetChannelSearchTimeoutPayload
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
  -- | RxExtMesgsEnable
  | EnableLED EnableLEDPayload
  | CrystalEnable CrystalEnablePayload
  | LibConfig LibConfigPayload
  | ConfigFrequencyAgility ConfigFrequencyAgilityPayload
  | SetProximitySearch SetProximitySearchPayload
  -- | ConfigEventBuffer{} -> 0x74
  | SetChannelSearchPriority SetChannelSearchPriorityPayload
  | Set128BitNetworkKey Set128BitNetworkKeyPayload
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
  | StartUpMessage StartUpMessagePayload
  -- | SerialErrorMessage{} -> 0xAE
  | ResetSystem
  | OpenChannel OpenChannelPayload
  | CloseChannel CloseChannelPayload
  | OpenRxScanMode OpenRxScanModePayload
  | BroadcastData BroadcastDataPayload
  | BurstData BurstDataPayload
  | AdvancedBurstData AdvancedBurstDataPayload
  | AcknowledgedData AcknowledgedDataPayload
  | ChannelResponse ChannelResponsePayload
  | ChannelStatus ChannelStatusPayload
  | ChannelId ChannelIdPayload
  | ANTVersion ANTVersionPayload
  | Capabilities CapabilitiesPayload
  | DeviceSerialNumber DeviceSerialNumberPayload
  deriving (Show)

--------------------------------------------------------------------------------
class IsAntMessage a where
  antMessage :: a -> AntMessage

--------------------------------------------------------------------------------
data UnAssignChannelPayload =
  UnAssignChannelPayload {unassignChannelNumber :: !ChannelNumber}
  deriving (Eq, Generic, Serial, Show)

instance IsAntMessage UnAssignChannelPayload where
  antMessage = UnAssignChannel

--------------------------------------------------------------------------------
newtype ChannelType =
  ChannelType Word8
  deriving (Eq,Generic,Serial,Show)

pattern Receive = ChannelType 0x00
pattern Transmit = ChannelType 0x10
pattern TransmitOnly = ChannelType 0x50
pattern ReceiveOnly = ChannelType 0x40
pattern SharedReceive = ChannelType 0x20
pattern SharedTransmit = ChannelType 0x30

--------------------------------------------------------------------------------
newtype ChannelNumber =
  ChannelNumber Word8
  deriving (Eq,Generic,Serial,Show)

--------------------------------------------------------------------------------
newtype NetworkNumber =
  NetworkNumber Word8
  deriving (Eq,Generic,Serial,Show)

--------------------------------------------------------------------------------
data AssignChannelPayload =
  AssignChannelPayload {assignChannelChannelNumber :: !ChannelNumber
                       ,assignChannelType :: !ChannelType
                       ,assignChannelNetworkNumber :: !NetworkNumber}
  deriving (Eq,Generic,Serial,Show)

instance IsAntMessage AssignChannelPayload where
  antMessage = AssignChannel

--------------------------------------------------------------------------------
data SetChannelIdPayload =
  SetChannelIdPayload {setChannelIdChannelNumber :: !ChannelNumber
                      ,setChannelIdDeviceNumber :: !DeviceNumber
                      ,setChannelIdDevicePairing :: Bool
                      ,setChannelIdDeviceType :: !Word8
                      ,setChannelIdTransmissionType :: !Word8}
  deriving (Show)

instance Serial SetChannelIdPayload where
  serialize SetChannelIdPayload{..} =
    do serialize setChannelIdChannelNumber
       serialize setChannelIdDeviceNumber
       putWord8 (bool (clearBit setChannelIdDeviceType 0)
                      (setBit setChannelIdDeviceType 0)
                      setChannelIdDevicePairing)
       putWord8 setChannelIdTransmissionType
  deserialize =
    do cn <- deserialize
       dn <- deserialize
       dt <- deserialize
       tt <- deserialize
       pure (SetChannelIdPayload cn
                                 dn
                                 (testBit dt 0)
                                 dt
                                 tt)

instance IsAntMessage SetChannelIdPayload where
  antMessage = SetChannelId

--------------------------------------------------------------------------------
data SetChannelPeriodPayload =
  SetChannelPeriodPayload {setChannelPeriodChannelNumber :: !ChannelNumber
                          ,setChannelPeriodMessagingPeriod :: !Word16}
  deriving (Show)

instance IsAntMessage SetChannelPeriodPayload where
  antMessage = SetChannelPeriod

instance Serial SetChannelPeriodPayload where
  serialize SetChannelPeriodPayload{..} =
    do serialize setChannelPeriodChannelNumber
       putWord16le setChannelPeriodMessagingPeriod
  deserialize = liftA2 SetChannelPeriodPayload deserialize getWord16le

--------------------------------------------------------------------------------
data SetChannelSearchTimeoutPayload =
  SetChannelSearchTimeoutPayload {setChannelSearchTimeoutChannelNumber :: !ChannelNumber
                                 ,setChannelSearchTimeoutSearchTimeout :: !Word8}
  deriving (Eq, Show, Generic, Serial)

instance IsAntMessage SetChannelSearchTimeoutPayload where
  antMessage = SetChannelSearchTimeout

--------------------------------------------------------------------------------
data SetChannelRFFreqPayload =
  SetChannelRFFreqPayload {setChannelRFFreqChannelNumber :: !ChannelNumber
                          ,setChannelRFFreqRFFrequency :: !Word8}
  deriving (Eq, Show, Generic, Serial)

instance IsAntMessage SetChannelRFFreqPayload where
  antMessage = SetChannelRFFreq

--------------------------------------------------------------------------------
data SetNetworkKeyPayload =
  SetNetworkKeyPayload {setNetworkKeyNetworkNumber :: !NetworkNumber
                       ,setNetworkKeyNetworkKey :: !NetworkKey}
  deriving (Eq,Show,Generic,Serial)

instance IsAntMessage SetNetworkKeyPayload where
  antMessage = SetNetworkKey

--------------------------------------------------------------------------------
data NetworkKey =
  NetworkKey !Word8
             !Word8
             !Word8
             !Word8
             !Word8
             !Word8
             !Word8
             !Word8
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
newtype PowerLevel = PowerLevel Word8
  deriving (Eq, Show, Generic, Serial)

pattern PowerLevel0 = PowerLevel 0
pattern PowerLevel1 = PowerLevel 1
pattern PowerLevel2 = PowerLevel 2
pattern PowerLevel3 = PowerLevel 3
pattern PowerLevel4 = PowerLevel 4

--------------------------------------------------------------------------------
data SetTransmitPowerPayload =
  SetTransmitPowerPayload {setTransmitPowerTransmitPower :: PowerLevel}
  deriving (Eq, Show, Generic, Serial)

--------------------------------------------------------------------------------
newtype SearchWaveform = SearchWaveform Word8
  deriving (Eq, Show, Generic, Serial)

pattern StandardSearchWaveform = SearchWaveform 0
pattern FastSearchWaveform = SearchWaveform 1

--------------------------------------------------------------------------------
data SetSearchWaveformPayload =
  SetSearchWaveformPayload {setSearchWaveformChannelNumber :: !ChannelNumber
                           ,setSearchWaveformSearchWaveform :: !SearchWaveform}
  deriving (Eq, Show, Generic, Serial)

--------------------------------------------------------------------------------
data SetChannelTxPowerPayload =
  SetChannelTxPowerPayload {setChannelTxPowerChannelNumber :: !ChannelNumber
                           ,setChannelTxPowerTransmitPower :: !PowerLevel}
  deriving (Eq, Show, Generic, Serial)

--------------------------------------------------------------------------------
data SetLowPriorityChannelSearchTimeoutPayload =
  SetLowPriorityChannelSearchTimeoutPayload {setLowPriorityChannelSearchTimeoutChannelNumber :: !ChannelNumber
                                            ,setLowPriorityChannelSearchTimeoutSearchTimeout :: !Word8}
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
data SetSerialNumChannelIdPayload =
  SetSerialNumChannelIdPayload {setSerialNumChannelIdChannelNumber :: !Word8
                               ,setSerialNumChannelIdPairingRequest :: !Bool
                               ,setSerialNumChannelIdDeviceTypeId :: !Word8
                               ,setSerialNumChannelIdTransmissionType :: !Word8}
  deriving (Show)

instance Serial SetSerialNumChannelIdPayload where
  serialize SetSerialNumChannelIdPayload{..} =
    do serialize setSerialNumChannelIdChannelNumber
       putWord8 (bool (clearBit setSerialNumChannelIdDeviceTypeId 0)
                      (setBit setSerialNumChannelIdDeviceTypeId 0)
                      setSerialNumChannelIdPairingRequest)
       putWord8 setSerialNumChannelIdTransmissionType
  deserialize =
    do cn <- deserialize
       dt <- deserialize
       tt <- deserialize
       pure (SetSerialNumChannelIdPayload cn
                                          (testBit dt 0)
                                          dt
                                          tt)

--------------------------------------------------------------------------------
data EnableExtendedMessagesPayload =
  EnableExtendedMessagesPayload {enableExtendedMessagesEnable :: !Bool}
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
data EnableLEDPayload =
  EnableLEDPayload {enableLEDEnable :: !Bool}
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
data CrystalEnablePayload =
  CrystalEnablePayload {crystalEnableEnable :: !Bool}
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
data LibConfigPayload =
  LibConfigPayload {libConfigEnableRxTimestamp :: !Bool
                   ,libConfigEnableRSSI :: !Bool
                   ,libConfigEnableChannelId :: !Bool}
  deriving (Show)

instance Serial LibConfigPayload where
  serialize (LibConfigPayload a b c) =
    putWord8 (0 .|. bool 0 32 a .|. bool 0 64 b .|. bool 0 128 c)
  deserialize =
    do byte <- getWord8
       pure (LibConfigPayload (byte .&. 32 == 32)
                              (byte .&. 64 == 64)
                              (byte .&. 128 == 128))

--------------------------------------------------------------------------------
data ConfigFrequencyAgilityPayload =
  ConfigFrequencyAgilityPayload {configFrequencyAgilityChannelNumber :: !ChannelNumber
                                ,configFrequencyAgilityUcFrequency1 :: !Word8
                                ,configFrequencyAgilityUcFrequency2 :: !Word8
                                ,configFrequencyAgilityUcFrequency3 :: !Word8}
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
newtype SearchThreshold = SearchThreshold Word8
  deriving (Eq,Show,Generic,Serial)

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

--------------------------------------------------------------------------------
data SetProximitySearchPayload =
  SetProximitySearchPayload {setProximitySearchPayloadChannelNumber :: !Word8
                            ,setProximitySearchPayloadUcSearchThreshold :: !(Maybe SearchThreshold)}
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
data SetChannelSearchPriorityPayload =
  SetChannelSearchPriorityPayload {setChannelSearchPriorityChannelNumber :: !Word8
                                  ,setChannelSearchPrioritySearchPriority :: !Word8}
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
data Set128BitNetworkKeyPayload =
  Set128BitNetworkKeyPayload {set128BitNetworkKeyNetworkNumber :: !Word8
                             ,set128BitNetworkKeyNetworkKey :: !NetworkKey128}
  deriving (Eq,Generic,Serial,Show)

data NetworkKey128 =
  NetworkKey128 !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
                !Word8
  deriving (Eq,Generic,Serial,Show)

--------------------------------------------------------------------------------
newtype Sync = Sync Word8
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
newtype MessageId = MessageId Word8
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
data StartUpMessagePayload =
  StartUpMessagePayload {startUpMessagePowerOnReset :: !Bool
                        ,startUpMessageHardwareResetLine :: !Bool
                        ,startUpMessageWatchDogReset :: !Bool
                        ,startUpMessageCommandReset :: !Bool
                        ,startUpMessageSynchronousReset :: !Bool
                        ,startUpMessageSuspendReset :: !Bool}
  deriving (Show)

instance Serial StartUpMessagePayload where
  serialize (StartUpMessagePayload a b c d e f) =
    putWord8 (if a
                 then 0
                 else foldl' (.|.)
                             0
                             (map (\(on,n) -> bool 0 (bit n) on)
                                  [(b,0),(c,1),(d,5),(e,6),(f,7)]))
  deserialize =
    do byte <- getWord8
       pure (StartUpMessagePayload (byte == 0)
                                   (testBit byte 0)
                                   (testBit byte 1)
                                   (testBit byte 5)
                                   (testBit byte 6)
                                   (testBit byte 7))

--------------------------------------------------------------------------------
data OpenChannelPayload =
  OpenChannelPayload {openChannelChannelNumber :: !ChannelNumber}
  deriving (Eq,Show,Generic,Serial)

instance IsAntMessage OpenChannelPayload where
  antMessage = OpenChannel

--------------------------------------------------------------------------------
data CloseChannelPayload =
  CloseChannelPayload {closeChannelChannelNumber :: !ChannelNumber}
  deriving (Eq,Show,Generic,Serial)

data OpenRxScanModePayload =
  OpenRxScanModePayload {openRxScanModeAllowSynchronousPacketsOnly :: !Bool}
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
data BroadcastDataPayload =
  BroadcastDataPayload {broadcastDataChannelNumber :: !ChannelNumber
                       ,broadcastDataData :: !BS.ByteString}
  deriving (Show)

--------------------------------------------------------------------------------
data AcknowledgedDataPayload =
  AcknowledgedDataPayload {acknowledgedDataChannelNumber :: !ChannelNumber
                          ,acknowledgedDataData :: !BS.ByteString}
  deriving (Show)

--------------------------------------------------------------------------------
newtype SequenceNumber = SequenceNumber Word8
  deriving (Show, Generic, Serial)

--------------------------------------------------------------------------------
data BurstDataPayload =
  BurstDataPayload {burstDataSequenceNumber :: !SequenceNumber
                   ,burstDataChannelNumber :: !ChannelNumber
                   ,burstDataData :: !BS.ByteString}
  deriving (Show)

--------------------------------------------------------------------------------
data AdvancedBurstDataPayload =
  AdvancedBurstDataPayload {advancedBurstDataSequenceNumber :: !SequenceNumber
                           ,advancedBurstDataChannelNumber :: !ChannelNumber}
                           -- ,advancedBurstDataData :: a}
  deriving (Show)

--------------------------------------------------------------------------------
newtype MessageCode = MessageCode Word8
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
data ChannelResponsePayload =
  ChannelResponsePayload {channelResponseChannelNumber :: !ChannelNumber
                         ,channelResponseMessageId :: !Word8
                         ,channelResponseMessageCode :: !MessageCode}
  deriving (Eq,Show,Generic,Serial)

--------------------------------------------------------------------------------
newtype ChannelState = ChannelState Word8
  deriving (Eq,Show)

--------------------------------------------------------------------------------
data ChannelStatusPayload =
  ChannelStatusPayload {channelStatusChannelNumber :: !ChannelNumber
                       ,channelStatusChannelType :: !ChannelType
                       ,channelStatusNetworkNumber :: !NetworkNumber
                       ,channelStatusChannelState :: !ChannelState}
  deriving (Eq,Show)

--------------------------------------------------------------------------------
newtype DeviceNumber = DeviceNumber Word16
  deriving (Show)

instance Serial DeviceNumber where
  serialize (DeviceNumber w16) = putWord16le w16
  deserialize = fmap DeviceNumber getWord16le

--------------------------------------------------------------------------------
newtype DeviceTypeId = DeviceTypeId Word8
  deriving (Show)

--------------------------------------------------------------------------------
newtype TransmissionType = TransmissionType Word8
  deriving (Show)

--------------------------------------------------------------------------------
data ChannelIdPayload =
  ChannelIdPayload {channelIdChannelNumber :: !ChannelNumber
                   ,channelIdDeviceNumber :: !DeviceNumber
                   ,channelIdDeviceTypeId :: !DeviceTypeId
                   ,channelIdTransmissionType :: !TransmissionType}
  deriving (Show)

--------------------------------------------------------------------------------
data ANTVersionPayload =
  ANTVersionPayload {antVersionVersion :: !Text}
  deriving (Eq,Generic,Serial,Show)

--------------------------------------------------------------------------------
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
  deriving (Show)

--------------------------------------------------------------------------------
data DeviceSerialNumberPayload =
  DeviceSerialNumberPayload {deviceSerialNumberSerialNumber :: !(Word8,Word8,Word8,Word8)}
  deriving (Eq, Generic, Serial,Show)

--------------------------------------------------------------------------------
sync :: Sync
sync = Sync 164

--------------------------------------------------------------------------------
data SerialMessage =
  SerialMessage {smSync :: !Sync
                ,smMessage:: !AntMessage}

--------------------------------------------------------------------------------
antEncodeMessage :: AntMessage -> Build.Builder
antEncodeMessage =
  \case
    ResetSystem -> Build.word8 0
    UnAssignChannel payload -> infer payload
    AssignChannel payload -> infer payload
    SetChannelId payload -> infer payload
    SetChannelPeriod payload -> infer payload
    SetChannelRFFreq payload -> infer payload
    SetNetworkKey payload -> infer payload
    OpenChannel payload -> infer payload
    SetTransmitPower payload -> infer payload
    SetSearchWaveform payload -> infer payload
    SetChannelTxPower payload -> infer payload
    SetLowPriorityChannelSearchTimeout payload -> infer payload
    SetSerialNumChannelId payload -> infer payload
    SetChannelSearchTimeout payload -> infer payload
    EnableLED payload -> infer payload
    CrystalEnable payload -> infer payload
    ConfigFrequencyAgility payload -> infer payload
    LibConfig payload -> infer payload
    StartUpMessage payload -> infer payload
    SetProximitySearch payload -> infer payload
    SetChannelSearchPriority payload -> infer payload
    Set128BitNetworkKey payload -> infer payload
    CloseChannel payload -> infer payload
    OpenRxScanMode payload -> infer payload
    BroadcastData BroadcastDataPayload{..} ->
      Build.lazyByteString
        (runPutL (do serialize broadcastDataChannelNumber
                     putByteString broadcastDataData))
    BurstData BurstDataPayload{..} ->
      Build.lazyByteString
        (runPutL (do serialize burstDataSequenceNumber
                     serialize burstDataChannelNumber
                     putByteString burstDataData))
    AcknowledgedData AcknowledgedDataPayload{..} ->
      Build.lazyByteString
        (runPutL (do serialize acknowledgedDataChannelNumber
                     putByteString acknowledgedDataData))
    -- AdvancedBurstData payload -> infer payload
    DeviceSerialNumber payload -> infer payload
  where infer :: Serial a
              => a -> Build.Builder
        infer = Build.lazyByteString . runPutL . serialize

--------------------------------------------------------------------------------
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
        EnableLED{} -> 0x68
        CrystalEnable{} -> 0x6D
        LibConfig{} -> 0x6E
        ConfigFrequencyAgility{} -> 0x70
        SetProximitySearch{} -> 0x71
        -- ConfigEventBuffer{} -> 0x74
        SetChannelSearchPriority{} -> 0x75
        Set128BitNetworkKey{} -> 0x76
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
        StartUpMessage{} -> 0x6F
        -- SerialErrorMessage{} -> 0xAE
        ResetSystem{} -> 0x4A
        OpenChannel{} -> 0x4B
        CloseChannel{} -> 0x4C
        OpenRxScanMode{} -> 0x5B
        BroadcastData{} -> 0x4E
        AcknowledgedData{} -> 0x4F
        BurstData{} -> 0x50
        AdvancedBurstData{} -> 0x72
        ChannelResponse{} -> 0x40
        ChannelStatus{} -> 0x52
        ChannelId{} -> 0x51
        ANTVersion{} -> 0x3E
        Capabilities{} -> 0x54
        DeviceSerialNumber{} -> 0x61

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
decodeSerialMessage
  :: BS.ByteString -> Either String SerialMessage
decodeSerialMessage =
  runGetS (do syncByte <- deserialize
              length <- getWord8
              messageId <- getWord8
              payload <-
                case messageId of
                  78 ->
                    BroadcastData <$>
                    (BroadcastDataPayload <$> deserialize <*> getBytes 8)
                  79 ->
                    AcknowledgedData <$>
                    (AcknowledgedDataPayload <$> deserialize <*> getBytes 8)
                  80 ->
                    BurstData <$>
                    (BurstDataPayload <$> deserialize <*> deserialize <*> getBytes 8)
                  64 -> ChannelResponse <$> deserialize
                  111 -> StartUpMessage <$> deserialize
                  _ -> fail ("Unknown message id " ++ show messageId)
              checksum <- getWord8
              isEmpty >>= guard
              return (SerialMessage syncByte payload))
