{-# LANGUAGE RecordWildCards #-}

module Main where

import ANTPlus.DeviceProfile.HeartRateMonitor
import ANTPlus.Network
import Control.Monad
import Control.Monad.Managed
import Data.Foldable (for_)
import Data.Monoid ((<>))
import qualified Data.Vector as V

main :: IO ()
main =
  do antStick <- fmap V.head findANTUSBDevices
     with (do liftIO (putStrLn "Opening ANT+")
              ant <-
                managed (withANTUSBDevice antStick)
              liftIO (putStrLn "Opened ANT+")
              network <-
                managed (withNetwork ant)
              liftIO (putStrLn "Opened network")
              managed (withHeartRateDevice network))
          (\hrm ->
             forever (do msg <- awaitHeartRatePage hrm
                         for_ msg
                              (\page ->
                                 putStrLn ("Heart rate: " <>
                                           show page))))
