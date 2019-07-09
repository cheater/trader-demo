module Data.Trader where

import Data.Time.Clock
  (UTCTime(..), NominalDiffTime(..), addUTCTime, getCurrentTime)
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Control.Concurrent.STM
  (TVar(..), STM(..), atomically, readTVar, readTVarIO, writeTVar)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Fixed (Pico, div')

type TradeSize = Integer
type Staged = Bool
type Deleted = Bool
type Executed = Bool

data TradeMeta =
  TradeMeta {
    tmTrade :: Trade
  , tmStaged :: Staged
  , tmDeleted :: Deleted
  }

data Trade =
    TradeTwap {
      tTwapSize :: TradeSize
    , tTwapPercentage :: Pico
    , tTwapStart :: UTCTime
    , tTwapEnd :: UTCTime
    , tTwapPeriod :: NominalDiffTime
    }
  | TradePov {
      tPovSize :: TradeSize
    , tPovPercentage :: Pico
    , tPovStart :: UTCTime
    , tPovEnd :: UTCTime
    , tPovPeriod :: NominalDiffTime
    }
  deriving (Eq)

data Order = Order {
    oTrade :: Trade
  , oDate :: UTCTime
  , oExecuted :: Executed
  , oStaged :: Staged
  , oDeleted :: Deleted
  , oMarketOrder :: Maybe MarketOrder
  }

data Volume =
  Volume {
    vSize :: MarketOrderAmount
  , vWindowStartTime :: UTCTime
  , vWindowEndTime :: UTCTime
  }

type MarketOrderAmount = Integer

data MarketOrder =
  MarketOrder {
    moAmount :: MarketOrderAmount
  , moExecutedAmount :: Maybe MarketOrderAmount
  }
  deriving (Show)

-- | Create 'UTCTime's in intervals of @period@, starting at @start@, and such
-- that they do not exceed @end@.
timeIntervals :: NominalDiffTime -> UTCTime -> UTCTime -> [UTCTime]
timeIntervals period start end = out
  where
    intervals = iterate (addUTCTime period) start
    out = takeWhile (<= end) intervals

-- helper for 'tradeToOrders'.
buildOrder :: Trade -> UTCTime -> Order
buildOrder trade date = Order {
    oTrade = trade
  , oDate = date
  , oExecuted = False
  , oStaged = True
  , oDeleted = False
  , oMarketOrder = Nothing
  }

-- | 'tradeToOrders' @t@ takes a 'Trade' and statically schedules 'Order's so
-- that, if they all trade at maximum size, they will reach the size of the
-- 'Trade' as close as possible.
--
-- NB. I've decided not to use RecordWildCards to make code simpler to follow.
tradeToOrders :: Trade -> [Order]
tradeToOrders t@(TradeTwap{}) = out
  where
    intervals = timeIntervals (tTwapPeriod t) (tTwapStart t) (tTwapEnd t)
    -- we don't want to overshoot 100% of the size:
    n = fromIntegral $ 1 `div'` (tTwapPercentage t)
    intervals2 = take n intervals
    out = (buildOrder t) <$> intervals2
tradeToOrders t@(TradePov{}) = out
  where
    intervals = timeIntervals (tPovPeriod t) (tPovStart t) (tPovEnd t)
    -- we don't want to overshoot 100% of the size:
    n = fromIntegral $ 1 `div'` (tPovPercentage t)
    intervals2 = take n intervals
    out = (buildOrder t) <$> intervals2


-- | Wait until the specified date of an 'Order'. This is the scheduling
-- subsystem of the Execution component.
waitForOrder :: TVar Order -> IO ()
waitForOrder orderM = do
  order <- readTVarIO orderM
  let target = oDate order
  now <- getCurrentTime
  when (now < target) $ do
    threadDelay 100000 -- 0.1 seconds
    waitForOrder orderM

-- | Generate a random market 'Volume' report. This is the Feeds part of the
-- system.
genMarketVolume :: IO Volume
genMarketVolume = do
  volume <- randomRIO (0, 1000000)
  now <- getCurrentTime
  let
    start = addUTCTime (fromInteger (-5)) now
    end = addUTCTime (fromInteger 5) now
  return $ Volume {
      vSize = volume
    , vWindowStartTime = start
    , vWindowEndTime = end
    }

-- | Get the minimum 'MarketOrder' size for a given market.
getMarketOrderMinSize :: IO MarketOrderAmount
getMarketOrderMinSize = return 2

-- | Get the sum of the executed amounts for all 'Order's for a given 'Trade'.
-- This is part of the Persistence element of the system.
getTradeOrdersSum :: Trade -> [TVar Order] -> STM MarketOrderAmount
getTradeOrdersSum trade allOrders = do
  orders <- getPlacedOrders trade allOrders
  return $ sumMarketOrders orders

-- | Get all 'Order's for a given 'Trade' that have been placed against the
-- market and executed.
getPlacedOrders :: Trade -> [TVar Order] -> STM [Order]
getPlacedOrders trade allOrders = catMaybes <$> mapM placedForTrade allOrders
  where
    placedForTrade :: TVar Order -> STM (Maybe Order)
    placedForTrade orderM = do
      order <- readTVar orderM
      return $ if (oTrade order) == trade
        then Just order
        else Nothing

-- | Sum the executed amounts for given 'Order's.
sumMarketOrders :: [Order] -> MarketOrderAmount
sumMarketOrders orders = sum $ map executedAmount orders
  where
    executedAmount order = case oMarketOrder order of
      Nothing -> 0
      Just marketOrder -> fromMaybe 0 (moExecutedAmount marketOrder)

-- | Look at a scheduled 'Order' and create a 'MarketOrder' against the market
-- based on reported market 'Volume'.
orderToMarketOrder
  :: MarketOrderAmount
  -> MarketOrderAmount
  -> [TVar Order]
  -> TVar Order
  -> STM (Maybe MarketOrder)
orderToMarketOrder minSize marketVolume allOrders orderM = do
  order <- readTVar orderM
  let trade = oTrade order
  tradeSum <- getTradeOrdersSum trade allOrders
  let
    restAmount = case trade of
      TradeTwap{} -> min
        ((tTwapSize trade) - tradeSum)
        (floor $ (fromIntegral $ tTwapSize trade) * (tTwapPercentage trade))
      TradePov{} -> min
        ((tPovSize trade) - tradeSum)
        (floor $ (fromIntegral marketVolume) * (tPovPercentage trade))
    amount = if restAmount > minSize then Just restAmount else Nothing
    marketOrder a = MarketOrder { moAmount = a, moExecutedAmount = Nothing }
  return $ marketOrder <$> amount

-- | Execute a 'MarketOrder' against the market, and report the executed trade.
executeMarketOrder :: MarketOrderAmount -> MarketOrder -> IO MarketOrder
executeMarketOrder minSize marketOrder = do
  let requested = moAmount marketOrder

  executed <- if requested >= minSize
    then Just <$> randomRIO (minSize, requested)
    else return Nothing
  putStrLn $
    "Placing order for: " <> (show requested) <> ", got: " <> (show executed)
  return marketOrder { moExecutedAmount = executed }

-- | Receive an 'Order' and place it against the market. This is the Execution
-- part of the system.
handleOrder :: [TVar Order] -> TVar Order -> IO ()
handleOrder allOrders orderM = do
  waitForOrder orderM
  putStrLn "Handling order."
  marketVolume <- genMarketVolume
  putStrLn $ "Market volume: " <> (show (vSize marketVolume))
  minSize <- getMarketOrderMinSize
  marketOrderM <- atomically $
    orderToMarketOrder minSize (vSize marketVolume) allOrders orderM
  -- in this code we assume that any orders are executed immediately, before we
  -- place new ones. Modelling maximum exposure for when orders are placed
  -- while pending orders still haven't been executed is possible, but a little
  -- complicated.
  executedOrder <- mapM (executeMarketOrder minSize) marketOrderM
  putStrLn $ "Executed order: " <> (show executedOrder)
  -- we assume that only one thread will be accessing a specific Order at a
  -- time. Otherwise we'd have to lock it, but we can forgo that here to make
  -- the code simpler.

  atomically $ do
    order <- readTVar orderM
    let order2 = order { oMarketOrder = executedOrder }
    writeTVar orderM order2

  putStrLn "Done executing order."
  putStrLn ""
