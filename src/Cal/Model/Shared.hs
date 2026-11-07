{-# LANGUAGE OverloadedStrings #-}
-- | Model pieces shared by several faces: month rows, forecast hints, the
-- rates panel, the currency bar and the day detail.
module Cal.Model.Shared
  ( ForecastHint (..)
  , forecastHint
  , MonthRow (..)
  , monthRows
  , RateRow (..)
  , RatesPanel (..)
  , ratesPanel
  , CurrencyBar (..)
  , currencyChoices
  , currencyBar
  , DetailRow (..)
  , TxnBlock (..)
  , txnBlocks
  , DetailNote (..)
  , DayDetail (..)
  , dayDetail
  ) where

import Cal.Currency (isCurrency)
import Cal.Journal (postingKind, postingLocation)
import Cal.Model.Common
import Cal.Period (lastDay, prevSpan, spanStatus)
import Cal.Prices (Prices (..), RateInfo (..), RateSource (..), rateInfo, rateOn, staleAge)
import Cal.Types
import Cal.Valuation
import Data.List (nub, partition, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Hledger hiding (Interval (..), Status)

-- * Forecast hints

data ForecastHint = ForecastHint
  { fhDescription :: !Text, fhNative :: !MixedAmount, fhValued :: !(Maybe Quantity), fhMore :: !Int }
  deriving (Eq, Show)

-- | A dim hint for an empty future span, from the periodic transactions.
-- Only without @--forecast@, for a Future span with no postings.
forecastHint :: Env -> Span -> Maybe ForecastHint
forecastHint env span'
  | sForecast s = Nothing
  | spanStatus (sToday s) span' /= Future = Nothing
  | not (null (periodPostings env span')) = Nothing
  | otherwise = case sortBy (comparing (Down . fromMaybe 0 . snd3)) candidates of
      [] -> Nothing
      (t, valued, native) : rest -> Just (ForecastHint (tdescription t) native valued (length rest))
  where
    s = envSettings env
    j = envJournal env
    prices = envPrices env
    dspan = DateSpan (Just (Exact (spStart span'))) (Just (Exact (spEnd span')))
    txns = concatMap (\pt -> runPeriodicTransaction False pt dspan) (jperiodictxns j)
    snd3 (_, b, _) = b
    candidates = mapMaybe candidate txns
    candidate t =
      let ps = [ p | p <- tpostings t, postingKind j p == Just KExpense
                   , matchesPostingExtra (journalAccountType j) (sQuery s) p ]
          native = maSum (map (mixedAmountStripCosts . pamount) ps)
      in if null ps then Nothing else Just (t, valuedAt native, native)
    valuedAt native = do
      b <- envBase env
      let v = mixedAmountValueAtDate (prOracle prices) (prStyles prices) (Just b) (lastDay span') native
      if all ((== 0) . aquantity) (amounts (foreignPart b v)) then Just (baseQty b v) else Nothing

-- * Month rows

data MonthRow = MonthRow
  { mrSpan :: !Span, mrStatus :: !Status, mrIncome, mrExpenses, mrNet :: !Money
  , mrInFrac, mrOutFrac :: !(Maybe Double), mrCommodities :: ![CommoditySymbol]
  , mrOverspent :: !Bool, mrHasPostings :: !Bool, mrHint :: !(Maybe ForecastHint) }
  deriving (Eq, Show)

-- | One row per span, each valued at the end of its own span, plus the bar
-- scale: the largest base income or expense figure (Nothing when Native or 0).
monthRows :: Env -> [Span] -> ([MonthRow], Maybe Quantity)
monthRows env spans = (map withFracs rows, scale)
  where
    base = envBase env
    rows = map row spans
    row sp =
      let vs = periodPostings env sp
          k = summarize env vs
          overspent = case base of
            Just b -> baseQty b (kExpenses k) > baseQty b (kIncome k)
            Nothing -> False
      in MonthRow
           { mrSpan = sp, mrStatus = spanStatus (sToday (envSettings env)) sp
           , mrIncome = kIncome k, mrExpenses = kExpenses k, mrNet = kNet k
           , mrInFrac = Nothing, mrOutFrac = Nothing
           , mrCommodities = aCommodities (kActivity k)
           , mrOverspent = overspent, mrHasPostings = not (null vs)
           , mrHint = forecastHint env sp }
    scale = do
      b <- base
      let m = maximum (0 : concat [[baseQty b (mrIncome r), baseQty b (mrExpenses r)] | r <- rows])
      if m > 0 then Just m else Nothing
    withFracs r = case (base, scale) of
      (Just b, Just sc) ->
        r { mrInFrac = Just (frac (baseQty b (mrIncome r)) sc)
          , mrOutFrac = Just (frac (baseQty b (mrExpenses r)) sc) }
      _ -> r
    frac x sc = realToFrac x / realToFrac sc

-- * Rates

data RateRow = RateRow
  { rtCommodity :: !CommoditySymbol, rtInfo :: !(Maybe RateInfo)
  , rtDeltaPct :: !(Maybe Double), rtStaleDays :: !(Maybe Integer) }
  deriving (Eq, Show)

data RatesPanel = RatesPanel { rpDate :: !Day, rpBase :: !CommoditySymbol, rpRows :: ![RateRow] }
  deriving (Eq, Show)

-- | The day rates are quoted on: the span's last day capped at today (a
-- future period end must not make rates look stale), or today for AtToday.
ratesDate :: Env -> Span -> Day
ratesDate env span' = case sRateAt s of
  AtToday -> sToday s
  _ -> min (lastDay span') (sToday s)
  where s = envSettings env

nativeCommodities :: VPosting -> [CommoditySymbol]
nativeCommodities v = map acommodity (amounts (vpNative v))

staleOn :: Env -> CommoditySymbol -> Day -> CommoditySymbol -> Maybe Integer
staleOn env base day c
  | c == base = Nothing
  | otherwise = rateInfo (envPrices env) c base day >>= staleAge (sStaleDays (envSettings env)) day

-- | Non-base commodities posted in the span plus stale currencies, in
-- 'currencyChoices' order with securities last. Δ is a fraction (0.02 = 2%).
ratesPanel :: Env -> Interval -> Span -> [VPosting] -> Maybe RatesPanel
ratesPanel env ivl span' vs = do
  base <- envBase env
  let day = ratesDate env span'
      prevDay = lastDay (prevSpan ivl span')
      choices = currencyChoices env
      posted = filter (/= base) (nub (concatMap nativeCommodities vs))
      stale = [c | c <- choices, isJust (staleOn env base day c)]
      wanted = nub (posted ++ stale)
      (currs, securities) = partition (`elem` choices) wanted
      ordered = filter (`elem` currs) choices ++ securities
      rateRow c =
        let info = rateInfo prices c base day
            staleDays = staleOn env base day c
            delta = case (info, staleDays, rateOn prices c base prevDay) of
              (Just i, Nothing, Just prev) | prev /= 0 ->
                Just (realToFrac (riRate i) / realToFrac prev - 1)
              _ -> Nothing
        in RateRow c info delta staleDays
  pure (RatesPanel day base (map rateRow ordered))
  where
    prices = envPrices env

-- * Currency bar

data CurrencyBar = CurrencyBar
  { cbChoices :: ![(CommoditySymbol, Bool)]
  , cbValuation :: !Valuation, cbDefault :: !CommoditySymbol, cbSource :: !BaseSource
  , cbRateAt :: !RateAt, cbPriceNote :: !(Maybe Text) }
  deriving (Eq, Show)

-- | The default base, then the other currencies by first appearance in the
-- journal's transaction postings (parse order).
currencyChoices :: Env -> [CommoditySymbol]
currencyChoices env = nub (dflt ++ others)
  where
    def' = sDefaultBase (envSettings env)
    dflt = [def' | def' /= ""]
    others =
      [ acommodity a
      | t <- jtxns (envJournal env), p <- tpostings t, a <- amounts (pamount p)
      , isCurrency (acommodity a) ]

currencyBar :: Env -> Span -> CurrencyBar
currencyBar env span' = CurrencyBar
  { cbChoices = [(c, stale c) | c <- currencyChoices env]
  , cbValuation = sValuation s, cbDefault = sDefaultBase s, cbSource = sBaseSource s
  , cbRateAt = sRateAt s, cbPriceNote = envBase env >>= priceNote }
  where
    s = envSettings env
    prices = envPrices env
    day = ratesDate env span'
    stale c = case envBase env of
      Nothing -> False
      Just b -> isJust (staleOn env b day c)
    -- the date each posting's rate is applied on
    appliedOn v = case sRateAt s of
      AtTxnDate -> vpDate v
      _ -> day
    priceNote b =
      let infos = [ (c, appliedOn v) | v <- periodPostings env span', c <- nativeCommodities v, c /= b ]
          srcs = concat [sources c b dt (3 :: Int) | (c, dt) <- nub infos]
      in case (SrcDeclared `elem` srcs, SrcInferred `elem` srcs) of
           (True, True) -> Just "market · inferred"
           (True, False) -> Just "market"
           (False, True) -> Just "inferred"
           _ -> Nothing
    -- declared/inferred sources behind a rate, following chained legs
    sources c b dt fuel = case rateInfo prices c b dt of
      Nothing -> []
      Just i -> case riSource i of
        SrcVia x
          | fuel > 0 -> sources c x dt (fuel - 1) ++ sources x b dt (fuel - 1)
          | otherwise -> []
        src -> [src]

-- * Day detail

data DetailRow = DetailRow
  { drAccount :: !AccountName, drLocation :: !(Maybe Text), drNative :: !MixedAmount
  , drRate :: !(Maybe Quantity), drValue :: !Money, drKind :: !Kind, drForecast :: !Bool }
  deriving (Eq, Show)

-- | tbFx = (the txn's total FX effect, txn-date rate of its largest-|fx|
-- posting), only when valued and the effect is non-zero.
data TxnBlock = TxnBlock { tbDescription :: !Text, tbRows :: ![DetailRow], tbFx :: !(Maybe (Quantity, Quantity)) }
  deriving (Eq, Show)

-- | Postings grouped by transaction, in the order the transactions first appear.
txnBlocks :: Env -> [VPosting] -> [TxnBlock]
txnBlocks env vs = map block (groupByTxn vs)
  where
    base = envBase env
    -- (first posting, all postings) per transaction
    groupByTxn = foldl ins []
      where
        ins acc v = case break ((== key v) . key . fst) acc of
          (before, (v0, g) : rest) -> before ++ (v0, g ++ [v]) : rest
          _ -> acc ++ [(v, [v])]
    key v = let t = vpTxn v in (tindex t, tdate t, tdescription t)
    block (v0, g) = TxnBlock
      { tbDescription = tdescription (vpTxn v0)
      , tbRows = map detailRow g
      , tbFx = base >>= fx g }
    detailRow v = DetailRow
      { drAccount = paccount (vpPosting v), drLocation = postingLocation (vpPosting v)
      , drNative = vpNative v, drRate = vpRate v, drValue = vpValue v
      , drKind = vpKind v, drForecast = vpForecast v }
    fx g b =
      let effects = [(v, fxEffect b v) | v <- g]
          total = sum (map snd effects)
      in case sortBy (comparing (Down . abs . snd)) effects of
           (top, _) : _
             | total /= 0
             , [c] <- nativeCommodities top ->
                 (,) total <$> rateOn (envPrices env) c b (vpDate top)
           _ -> Nothing

data DetailNote = NoteFx !Quantity !Money !Quantity | NoteNoConversion !CommoditySymbol | NoteNone
  deriving (Eq, Show)

data DayDetail = DayDetail
  { ddDay :: !Day, ddTxns :: ![TxnBlock], ddPostings :: !Int
  , ddCommodities :: ![CommoditySymbol], ddNote :: !DetailNote }
  deriving (Eq, Show)

-- | The day's postings, valued with the span as valuation period.
dayDetail :: Env -> Span -> Day -> DayDetail
dayDetail env span' day = DayDetail
  { ddDay = day, ddTxns = txnBlocks env vs, ddPostings = length vs
  , ddCommodities = aCommodities (kActivity (summarize env vs))
  , ddNote = maybe NoteNone note (envBase env) }
  where
    vs = filter ((== day) . vpDate) (periodPostings env span')
    note b
      | not (null vs) && all (all (== b) . nativeCommodities) vs = NoteNoConversion b
      | otherwise =
          let groups = M.toList (M.fromListWith (flip (++)) [(c, [v]) | v <- vs, [c] <- [nativeCommodities v]])
              withFx = [(c, g, sum (map (fxEffect b) g)) | (c, g) <- groups]
              total = sum [x | (_, _, x) <- withFx]
          in case sortBy (comparing (\(_, _, x) -> Down (abs x))) withFx of
               (c, g, x) : _
                 | total /= 0
                 , Just r <- rateOn (envPrices env) c b day ->
                     NoteFx r (maSum (map vpAtThen g)) x
               _ -> NoteNone
