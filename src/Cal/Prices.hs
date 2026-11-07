module Cal.Prices
  ( PricePoint (..)
  , Prices (..)
  , RateSource (..)
  , RateInfo (..)
  , mkPrices
  , rateOn
  , rateInfo
  , staleAge
  ) where

import Cal.Types (Day)
import Data.List (maximumBy)
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import Data.Time.Calendar (diffDays)
import Hledger.Data.Journal (journalCommodityStyles, journalInferMarketPricesFromTransactions)
import Hledger.Data.Types
import Hledger.Data.Valuation (PriceOracle, journalPriceOracle)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)

data PricePoint = PricePoint
  { ppDate :: !Day
  , ppFrom, ppTo :: !CommoditySymbol
  , ppRate :: !Quantity
  , ppDeclared :: !Bool
  }
  deriving (Eq, Show)

data Prices = Prices
  { prOracle :: PriceOracle
  , prPoints :: [PricePoint]
  , prStyles :: Map CommoditySymbol AmountStyle
  }

data RateSource = SrcDeclared | SrcInferred | SrcVia !CommoditySymbol
  deriving (Eq, Show)

data RateInfo = RateInfo
  { riRate :: !Quantity
  , riSource :: !RateSource
  , riDate :: !Day
  }
  deriving (Eq, Show)

-- | Bool = infer market prices from transactions.
mkPrices :: Bool -> Journal -> Prices
mkPrices infer j = Prices
  { prOracle = journalPriceOracle infer j
  , prPoints = declared ++ inferred
  , prStyles = journalCommodityStyles j
  }
  where
    declared =
      [ PricePoint (pddate pd) (pdcommodity pd) (acommodity a) (aquantity a) True
      | pd <- jpricedirectives j
      , let a = pdamount pd
      ]
    inferred
      | infer =
          [ PricePoint (mpdate mp) (mpfrom mp) (mpto mp) (mprate mp) False
          | mp <- jinferredmarketprices (journalInferMarketPricesFromTransactions j)
          ]
      | otherwise = []

-- | Units of @to@ per 1 @from@.
rateOn :: Prices -> CommoditySymbol -> CommoditySymbol -> Day -> Maybe Quantity
rateOn p from to day = snd <$> prOracle p (day, from, Just to)

rateInfo :: Prices -> CommoditySymbol -> CommoditySymbol -> Day -> Maybe RateInfo
rateInfo p from to day = do
  rate <- rateOn p from to day
  let cands = filter ((<= day) . ppDate) (prPoints p)
      links a b pt = (ppFrom pt == a && ppTo pt == b) || (ppFrom pt == b && ppTo pt == a)
      -- latest by date; on a tie prefer declared
      latest = maximumBy (comparing (\pt -> (ppDate pt, ppDeclared pt)))
      latestLink a b = case filter (links a b) cands of
        [] -> Nothing
        xs -> Just (latest xs)
      other a pt = if ppFrom pt == a then ppTo pt else ppFrom pt
      direct = latestLink from to
      viaCands =
        [ (x, min (ppDate l1) (ppDate l2))
        | x <- uniq (map (other from) (filter (\pt -> ppFrom pt == from || ppTo pt == from) cands))
        , x /= from, x /= to
        , Just l1 <- [latestLink from x]
        , Just l2 <- [latestLink x to]
        ]
      uniq = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
  case direct of
    Just pt -> pure (RateInfo rate (if ppDeclared pt then SrcDeclared else SrcInferred) (ppDate pt))
    Nothing -> do
      (x, d) <- listToMaybe (reverse (sortOn snd viaCands))
      pure (RateInfo rate (SrcVia x) d)

-- | @Just (asOf - riDate)@ when older than @staleDays@.
staleAge :: Integer -> Day -> RateInfo -> Maybe Integer
staleAge staleDays asOf info
  | n > staleDays = Just n
  | otherwise = Nothing
  where n = diffDays asOf (riDate info)
