module Main where

import Data.Trader
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (newTVar, atomically)


main :: IO ()
main = do
  -- create two orders: TWAP and POV.
  now <- getCurrentTime
  let
    twapStart = addUTCTime (fromInteger 5) now
    twapEnd = addUTCTime (fromInteger 10) now
    twapPeriod = fromInteger 1
    trade1 = TradeMeta {
        tmTrade = TradeTwap {
            tTwapSize = 1000
          , tTwapPercentage = 0.20
          , tTwapStart = twapStart
          , tTwapEnd = twapEnd
          , tTwapPeriod = twapPeriod
          }
      , tmStaged = True
      , tmDeleted = False
      }

    povStart = addUTCTime (fromInteger 15) now
    povEnd = addUTCTime (fromInteger 20) now
    povPeriod = fromInteger 1
    trade2 = TradeMeta {
        tmTrade = TradePov {
            tPovSize = 5000
          , tPovPercentage = 0.10
          , tPovStart = povStart
          , tPovEnd = povEnd
          , tPovPeriod = povPeriod
          }
      , tmStaged = True
      , tmDeleted = False
      }

    orders =
      (tradeToOrders $ tmTrade trade1) ++ (tradeToOrders $ tmTrade trade2)

  -- create a concurrently-accessible database of orders, simulating the
  -- Persistence component of the design.
  ordersStm <- mapM (atomically . newTVar) orders

  mapConcurrently_ (handleOrder ordersStm) ordersStm

  let trade1Target = (tTwapSize . tmTrade) trade1
  trade1Executed <- atomically $ getTradeOrdersSum (tmTrade trade1) ordersStm
  putStrLn $
    "trade1: target: " <> (show trade1Target) <>
    ", executed: " <> (show trade1Executed)

  let trade2Target = (tPovSize . tmTrade) trade2
  trade2Executed <- atomically $ getTradeOrdersSum (tmTrade trade2) ordersStm
  putStrLn $
    "trade2: target: " <> (show trade2Target) <>
    ", executed: " <> (show trade2Executed)
