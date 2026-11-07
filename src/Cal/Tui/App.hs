-- | The interactive brick application: drawing, attributes and the event loop.
module Cal.Tui.App
  ( runTui
  , Name (..)
  , blockWidget
  , calAttrMap
  , attrMapFor
  , styleAttr
  , clampRegisterScroll
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeAsyncException, displayException, fromException, throwIO, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word (Word8)

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import qualified Graphics.Vty as Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Hledger.Data.Types (Journal)
import Hledger.Read.InputOptions (InputOpts)

import Cal.Doc
import Cal.Journal (loadJournal)
import Cal.Model.Common (Env (..))
import Cal.Model.Register (RegisterRow, registerRows)
import Cal.Period (faceSpan)
import Cal.Theme (RGB, Role (..), backgroundRGB, roleRGB, tintRGB)
import Cal.Tui.State
import Cal.Types (ColorMode (..), GlyphSet (..), Settings (..))
import Cal.View (faceDoc)
import Cal.View.Chrome (footerBlock)
import Cal.View.Register (registerPane, registerVisibleRows)
import Cal.View.Types (RenderCtx (..), ViewDoc (..), mkRenderCtx)

data Name = BodyViewport deriving (Eq, Ord, Show)

data Tick = Tick

-- | The UI state and the time of the latest event (for message expiry).
data AppState = AppState !UiState !UTCTime

-- * Attributes

tints :: [Int]
tints = [100, 60, 40, 25]

roleName :: Role -> String
roleName r = map toLower (drop 1 (show r))

-- | @role.<role>.<tint>[.bold][.inverse]@
styleAttr :: Style -> AttrName
styleAttr s = mconcat (map attrName parts)
  where
    parts = ["role", roleName (stRole s), show (stTint s)]
      ++ ["bold" | stBold s] ++ ["inverse" | stInverse s]

allStyles :: [Style]
allStyles =
  [ Style r t b i | r <- [minBound .. maxBound], t <- tints, b <- [False, True], i <- [False, True] ]

rgb :: RGB -> Vty.Color
rgb (r, g, b) = Vty.rgbColor (w r) (w g) (w b)
  where w = fromIntegral :: Word8 -> Int

styleVtyAttr :: ColorMode -> Style -> Vty.Attr
styleVtyAttr cm s = withBold (colours Vty.defAttr)
  where
    withBold a = if stBold s then a `Vty.withStyle` Vty.bold else a
    c = tintRGB (stTint s) (roleRGB (stRole s))
    colours a
      | cm == ColorNone = if stInverse s then a `Vty.withStyle` Vty.reverseVideo else a
      | stInverse s = a `Vty.withForeColor` rgb backgroundRGB `Vty.withBackColor` rgb c
      | stRole s == RPanel = a `Vty.withForeColor` rgb (roleRGB RText) `Vty.withBackColor` rgb c
      | otherwise = a `Vty.withForeColor` rgb c

-- | Every role x tint x bold x inverse; no colours at all for 'ColorNone'.
attrMapFor :: ColorMode -> AttrMap
attrMapFor cm = attrMap Vty.defAttr [ (styleAttr s, styleVtyAttr cm s) | s <- allStyles ]

calAttrMap :: AttrMap
calAttrMap = attrMapFor ColorTrue

-- * Widgets

lineWidget :: Line -> Widget Name
lineWidget (Line ss) = case [ withAttr (styleAttr s) (txt t) | Seg s t <- ss, not (T.null t) ] of
  [] -> txt " "
  ws -> hBox ws

blockWidget :: Block -> Widget Name
blockWidget b = vBox (map lineWidget (bLines b))

-- * Layout

data Layout = Layout
  { lyCtx :: RenderCtx, lyDoc :: ViewDoc, lyRest :: Block, lyFooter :: Block
  , lyRegHeight :: Int, lyRegRows :: [RegisterRow] }

blankLine :: Block
blankLine = Block 0 [Line []]

-- | Everything the screen needs at a terminal size; Nothing below 80 columns.
layout :: UTCTime -> UiState -> Int -> Int -> Maybe Layout
layout now us w h = do
  ctx0 <- mkRenderCtx env w True
  let ctx = ctx0 { rcMessage = visibleMessage now us }
      doc = faceDoc ctx env (usCursor us)
      keysFooter = footerBlock ctx (vdKeys doc) (vdLegend doc)
      -- Every view ends its bottom block with the footer; drop it, we draw our own.
      bl = bLines (vdBottom doc)
      fl = bLines keysFooter
      cut = length bl - length fl
      rest = if cut >= 0 && drop cut bl == fl then (vdBottom doc) { bLines = take cut bl } else vdBottom doc
      footer = case usPane us of
        PaneFilter draft _ -> line [Seg (st RDim) "/ ", Seg (st RText) draft, Seg (st RCursor) cursorGlyph]
        _ -> keysFooter
      topH = length (bLines (vdTop doc))
      regH = max 4 (h - topH - 1 - length (bLines footer))
      s = envSettings env
      rows = registerRows env (faceSpan (sFace s) (sCommInterval s) (sFocus s)) (sFocus s)
  pure (Layout ctx doc rest footer regH rows)
  where
    env = usEnv us
    cursorGlyph = if sGlyphSet (envSettings env) == GlyphsAscii then "_" else "▏"

-- | Keep the register scroll offset within its rows.
clampScroll :: Layout -> UiState -> UiState
clampScroll ly us = case usPane us of
  PaneRegister i -> us { usPane = PaneRegister (clampRegisterScroll (lyRegHeight ly) (length (lyRegRows ly)) i) }
  _ -> us

-- | @clampRegisterScroll height rowCount scroll@: 0 .. rows that don't fit.
clampRegisterScroll :: Int -> Int -> Int -> Int
clampRegisterScroll height n i = max 0 (min (max 0 (n - registerVisibleRows height)) i)

draw :: AppState -> [Widget Name]
draw (AppState us now) = [helpLayer | usPane us == PaneHelp] ++ [screen]
  where
    screen = Widget Greedy Greedy $ do
      c <- getContext
      render (screenAt (availWidth c) (availHeight c))
    screenAt w h = case layout now us w h of
      Nothing -> txt ("hledger cal needs at least 80 columns (have " <> T.pack (show w) <> ")")
      Just ly ->
        let doc = lyDoc ly
            ctx = lyCtx ly
            body = viewport BodyViewport Vertical $ vBox
              [ if Just i == vdCursorRow doc then visible (lineWidget l) else lineWidget l
              | (i, l) <- zip [0 ..] (bLines (vdBody doc)) ]
            middle = case usPane us of
              PaneRegister scroll ->
                [ blockWidget (registerPane ctx (sFocus (rcSettings ctx)) (lyRegHeight ly) scroll (lyRegRows ly)) ]
              _ -> [body, blockWidget blankLine] ++ [blockWidget (lyRest ly) | not (null (bLines (lyRest ly)))]
        in vBox ([blockWidget (vdTop doc), blockWidget blankLine] ++ middle ++ [blockWidget (lyFooter ly)])
    helpLayer = Widget Fixed Fixed $ do
      c <- getContext
      let w = availWidth c
          s = envSettings (usEnv us)
          ascii = sGlyphSet s == GlyphsAscii
          sep = if ascii then " . " else " · "
          enter = if ascii then "Enter" else "⏎"
          general = T.intercalate sep
            [ "Y Q M W C view", "Tab next view", "[ ] period", "c currency", "v rates at"
            , "/ filter", enter <> " open", "Esc back", "g reload", "? help", "q quit" ]
          faceKeys = case layout now us w (availHeight c) of
            Just ly -> T.intercalate sep [ k <> " " <> l | (k, l) <- vdKeys (lyDoc ly) ]
            Nothing -> ""
      render $ centerLayer $ borderWithLabel (txt "keys") $ hLimit (max 20 (min 100 (w - 4))) $
        padLeftRight 1 $ vBox ([txtWrap general] ++ [txtWrap faceKeys | not (T.null faceKeys)])

-- * Events

displaySize :: EventM Name AppState (Int, Int)
displaySize = do
  vty <- getVtyHandle
  liftIO (Vty.displayBounds (Vty.outputIface vty))

-- | Clamp the register scroll to the current screen.
settle :: UTCTime -> UiState -> EventM Name AppState ()
settle now us = do
  (w, h) <- displaySize
  put (AppState (maybe us (`clampScroll` us) (layout now us w h)) now)

-- | Reload; synchronous exceptions become an error message, async ones are rethrown.
reloadJournal :: InputOpts -> [FilePath] -> IO (Either String Journal)
reloadJournal opts files = do
  r <- try (loadJournal opts files)
  case r of
    Left e | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
           | otherwise -> pure (Left (displayException e))
    Right x -> pure x

handleEvent :: InputOpts -> [FilePath] -> BrickEvent Name Tick -> EventM Name AppState ()
handleEvent opts files ev = do
  now <- liftIO getCurrentTime
  AppState us _ <- get
  case ev of
    AppEvent Tick -> put (AppState us now)
    VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) -> halt
    VtyEvent (Vty.EvKey k mods) -> case keyCmd (usPane us) k mods of
      Nothing -> put (AppState us now)
      Just cmd -> case step now cmd us of
        (_, EffQuit) -> halt
        (us', EffReload) -> do
          r <- liftIO (reloadJournal opts files)
          settle now (applyReload now r us')
        (us', EffNone) -> settle now us'
    VtyEvent (Vty.EvResize _ _) -> settle now us
    _ -> pure ()

app :: InputOpts -> [FilePath] -> ColorMode -> App AppState Tick Name
app opts files cm = App
  { appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent opts files
  , appStartEvent = pure ()
  , appAttrMap = const (attrMapFor cm)
  }

-- | Run the interactive calendar; @opts@ and @files@ (every @-f@, as at
-- startup) are reused for @g@ (reload).
runTui :: InputOpts -> [FilePath] -> Env -> IO ()
runTui opts files env = do
  chan <- newBChan 10
  void $ forkIO $ forever $ writeBChan chan Tick >> threadDelay 500000
  now <- getCurrentTime
  let build = mkVty Vty.defaultConfig
  vty <- build
  void $ customMain vty build (Just chan) (app opts files (sColor (envSettings env))) (AppState (initState env) now)
