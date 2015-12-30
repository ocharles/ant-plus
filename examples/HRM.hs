{-# LANGUAGE RecordWildCards #-}

module Main where

import ANTPlus.DeviceProfile.HeartRateMonitor
import ANTPlus.Network
import Control.Monad
import Control.Monad.Managed
import Data.Monoid ((<>))
import qualified Data.Vector as V
import Control.Concurrent.STM

main :: IO ()
main =
  do antStick <- fmap V.head findANTUSBDevices
     with (do liftIO (putStrLn "Opening ANT+")
              ant <- managed (withANTUSBDevice antStick)
              liftIO (putStrLn "Opened ANT+")
              network <- managed (withNetwork ant)
              liftIO (putStrLn "Opened network")
              managed (withHeartRateMonitor network))
          (\hrm ->
             do awaitPage <- newHeartRatePageReader hrm
                forever (do page <- atomically awaitPage
                            putStrLn ("Heart rate: " <> show page)))
