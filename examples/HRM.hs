{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import ANTPlus.DeviceProfile.BicyclePower
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
              hrm <- managed (withHeartRateMonitor network)
              pwr <- managed (withBicyclePower network)
              pure (hrm,pwr))
          (\(hrm,pwr) ->
             do awaitHrmPage <- newHeartRatePageReader hrm
                awaitPwrPage <- newBicyclePowerPageReader pwr
                forever (do page <-
                              atomically
                                (fmap Left awaitHrmPage <|>
                                 fmap Right awaitPwrPage)
                            print page))
