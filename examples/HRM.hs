{-# LANGUAGE RecordWildCards #-}

module Main where

import ANTPlus.DeviceProfile.HeartRateMonitor
import ANTPlus.Network
import Control.Monad
import Control.Monad.Managed
import Data.Foldable (for_)
import Data.Monoid ((<>))
import System.USB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V

main :: IO ()
main =
  do antStick <- fmap V.head findANTUSBDevices
     with (do ant <-
                managed (withANTUSBDevice antStick)
              network <-
                managed (withNetwork ant)
              managed (withHeartRateDevice network))
          (\hrm ->
             forever (do msg <- awaitHeartRatePage hrm
                         for_ msg
                              (\HeartRatePage{..} ->
                                 putStrLn ("Heart rate: " <>
                                           show hrmComputedHeartRate))))
