-- | The interactive brick application: drawing, attributes and the event loop.
module Cal.Tui.App
  ( runTui
  , Name (..)
  , blockWidget
  , calAttrMap
  , attrMapFor
  , styleAttr
  , bgAttr
  , clampRegisterScroll
  , cursorSpan
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
import Cal.Glyphs (Glyphs (..), glyphsFor)
import Cal.Types (ColorMode (..), GlyphSet (..), Settings (..))
import Cal.View (faceDoc)
import Cal.View.Chrome (footerBlock)
import Cal.View.Register (registerPane, registerVisibleRows)
import Cal.View.Types (RenderCtx (..), ViewDoc (..), mkRenderCtx)

data Name = BodyViewport deriving (Eq, Ord, Show)

data Tick = Tick

-- | The UI state, the time of the latest event (for message expiry) and the
-- layout for the latest screen size. The layout is rebuilt only when the
-- state, the size or the visible message changes, and is shared by the
-- screen and the help overlay.
data AppState = AppState !UiState !UTCTime !(Maybe Cache)

-- | A layout and the screen size it was made for (lazy: built on first use).
data Cache = Cache !(Int, Int) (Maybe Layout)

-- * Attributes

tints :: [Int]
tints = [100, 60, 40, 25]

roleName :: Role -> String
roleName r = map toLower (drop 1 (show r))

-- | @role.<role>.<tint>[.bold][.inverse]@; the background is a separate
-- attribute ('bgAttr').
styleAttr :: Style -> AttrName
styleAttr s = mconcat (map attrName parts)
  where
    parts = ["role", roleName (stRole s), show (stTint s)]
      ++ ["bold" | stBold s] ++ ["inverse" | stInverse s]

-- | @bg.<role>.<tint>@: only a background colour, laid under a style's
-- attribute with 'withDefAttr'.
bgAttr :: Role -> Int -> AttrName
bgAttr r t = mconcat (map attrName ["bg", roleName r, show t])

allStyles :: [Style]
allStyles =
  [ Style r t b i Nothing | r <- [minBound .. maxBound], t <- tints, b <- [False, True], i <- [False, True] ]

rgb :: RGB -> Vty.Color
rgb (r, g, b) = Vty.rgbColor (w r) (w g) (w b)
  where w = fromIntegral :: Word8 -> Int

styleVtyAttr :: ColorMode -> Style -> Vty.Attr
styleVtyAttr cm s = withBold (colours Vty.defAttr)
  where
    -- the cursor border is bold where there is no colour (spec §10.2)
    withBold a
      | stBold s || (cm == ColorNone && stRole s == RCursorBorder) = a `Vty.withStyle` Vty.bold
      | otherwise = a
    c = tintRGB (stTint s) (roleRGB (stRole s))
    colours a
      | cm == ColorNone = if stInverse s then a `Vty.withStyle` Vty.reverseVideo else a
      | stInverse s = a `Vty.withForeColor` rgb backgroundRGB `Vty.withBackColor` rgb c
      | stRole s == RPanel = a `Vty.withForeColor` rgb (roleRGB RText) `Vty.withBackColor` rgb c
      | otherwise = a `Vty.withForeColor` rgb c

bgVtyAttr :: ColorMode -> Role -> Int -> Vty.Attr
bgVtyAttr cm r t
  | cm == ColorNone = Vty.defAttr
  | otherwise = Vty.defAttr `Vty.withBackColor` rgb (tintRGB t (roleRGB r))

-- | Every role x tint x bold x inverse, and every role x tint as a
-- background; no colours at all for 'ColorNone'.
attrMapFor :: ColorMode -> AttrMap
attrMapFor cm = attrMap Vty.defAttr $
  [ (styleAttr s, styleVtyAttr cm s) | s <- allStyles ]
  ++ [ (bgAttr r t, bgVtyAttr cm r t) | r <- [minBound .. maxBound], t <- tints ]

calAttrMap :: AttrMap
calAttrMap = attrMapFor ColorTrue

-- * Widgets

lineWidget :: Line -> Widget Name
lineWidget (Line ss) = case [ withBg s (withAttr (styleAttr s) (txt t)) | Seg s t <- ss, not (T.null t) ] of
  [] -> txt " "
  ws -> hBox ws

-- | Lay the style's background (if any) under it; reverse video has its own.
withBg :: Style -> Widget Name -> Widget Name
withBg s w = case stBg s of
  Just (r, t) | not (stInverse s) -> withDefAttr (bgAttr r t) w
  _ -> w

blockWidget :: Block -> Widget Name
blockWidget b = vBox (map lineWidget (bLines b))

-- * Layout

data Layout = Layout
  { lyCtx :: RenderCtx, lyDoc :: ViewDoc, lyRest :: Block, lyFooter :: Block
  , lyRegHeight :: Int, lyRegRows :: [RegisterRow] }

blankLine :: Block
blankLine = Block 0 [Line []]

-- | Everything the screen needs at a terminal size; Nothing below 80 columns.
-- Views leave the key-hint footer to us.
layout :: UTCTime -> UiState -> Int -> Int -> Maybe Layout
layout now us w h = do
  ctx0 <- mkRenderCtx env w True
  let ctx = ctx0 { rcMessage = visibleMessage now us }
      doc = faceDoc ctx env (usCursor us)
      keysFooter = footerBlock ctx (vdKeys doc) (vdLegend doc)
      rest = vdBottom doc
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

-- | The cached layout when it was made for this size, else a fresh one.
layoutFor :: AppState -> Int -> Int -> Maybe Layout
layoutFor (AppState us now cache) w h = case cache of
  Just (Cache size ly) | size == (w, h) -> ly
  _ -> layout now us w h

draw :: AppState -> [Widget Name]
draw as@(AppState us _ _) = [helpLayer | usPane us == PaneHelp] ++ [screen]
  where
    screen = Widget Greedy Greedy $ do
      c <- getContext
      render (screenAt (availWidth c) (availHeight c))
    screenAt w h = case layoutFor as w h of
      Nothing -> txt ("hledger cal needs at least 80 columns (have " <> T.pack (show w) <> ")")
      Just ly ->
        let doc = lyDoc ly
            ctx = lyCtx ly
            bodyLines = bLines (vdBody doc)
            rest = bLines (lyRest ly)
            -- the panels sit one blank row under the body (design S3); the
            -- footer stays at the bottom, the space left over goes above it
            restH = if null rest then 0 else length rest + 1
            room = max 1 (h - length (bLines (vdTop doc)) - 2 - restH - length (bLines (lyFooter ly)))
            bodyH = min room (length bodyLines)
            -- the viewport follows the cursor: its row and the whole box drawn around it
            body = vLimit bodyH $ viewport BodyViewport Vertical $ vBox $ case cursorSpan (vdCursorRow doc) bodyLines of
              Nothing -> map lineWidget bodyLines
              Just (lo, hi) ->
                let (before, rest') = splitAt lo bodyLines
                    (inside, after) = splitAt (hi - lo + 1) rest'
                in map lineWidget before ++ [visible (vBox (map lineWidget inside))] ++ map lineWidget after
            filler = blockWidget (Block 0 (replicate (room - bodyH) (Line [])))
            middle = case usPane us of
              PaneRegister scroll ->
                [ blockWidget (registerPane ctx (sFocus (rcSettings ctx)) (lyRegHeight ly) scroll (lyRegRows ly)) ]
              _ -> [body, blockWidget blankLine]
                ++ concat [ [blockWidget (lyRest ly), blockWidget blankLine] | not (null rest) ]
                ++ [filler | room > bodyH]
        in vBox ([blockWidget (vdTop doc), blockWidget blankLine] ++ middle ++ [blockWidget (lyFooter ly)])
    helpLayer = Widget Fixed Fixed $ do
      c <- getContext
      let w = availWidth c
          s = envSettings (usEnv us)
          gl = glyphsFor (sGlyphSet s)
          sep = gSep gl
          enter = gKeyEnter gl
          general = T.intercalate sep
            [ "Y Q M W C view", "Tab next view", "[ ] period", "c currency", "v rates at"
            , "/ filter", enter <> " open", "Esc back", "g reload", "? help", "q quit" ]
          faceKeys = case layoutFor as w (availHeight c) of
            Just ly -> T.intercalate sep [ k <> " " <> l | (k, l) <- vdKeys (lyDoc ly) ]
            Nothing -> ""
      render $ centerLayer $ borderWithLabel (txt "keys") $ hLimit (max 20 (min 100 (w - 4))) $
        padLeftRight 1 $ vBox ([txtWrap general] ++ [txtWrap faceKeys | not (T.null faceKeys)])

-- | The body lines the viewport keeps in view, first and last: the cursor
-- row and, when it holds the cursor border, the lines on either side of it
-- that hold it too, so a cursor box (design S4) scrolls into view whole.
cursorSpan :: Maybe Int -> [Line] -> Maybe (Int, Int)
cursorSpan Nothing _ = Nothing
cursorSpan (Just r) ls
  | has r = Just (lo, hi)
  | otherwise = Just (r, r)
  where
    marked = [ any (\(Seg s _) -> stRole s == RCursorBorder) ss | Line ss <- ls ]
    has i = i >= 0 && i < length marked && marked !! i
    lo = last (r : takeWhile has [r - 1, r - 2 .. 0])
    hi = last (r : takeWhile has [r + 1 .. length marked - 1])

-- * Events

displaySize :: EventM Name AppState (Int, Int)
displaySize = do
  vty <- getVtyHandle
  liftIO (Vty.displayBounds (Vty.outputIface vty))

-- | Store a new state with its layout, clamping the register scroll to the
-- screen (the scroll offset doesn't change the layout).
settle :: UTCTime -> UiState -> EventM Name AppState ()
settle now us = do
  (w, h) <- displaySize
  let us' = us { usWidth = w }
      ly = layout now us' w h
  put (AppState (maybe us' (`clampScroll` us') ly) now (Just (Cache (w, h) ly)))

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
  AppState us before _ <- get
  case ev of
    -- redraw only when a message has just expired
    AppEvent Tick
      | visibleMessage now us /= visibleMessage before us -> settle now us
      | otherwise -> continueWithoutRedraw
    VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) -> halt
    VtyEvent (Vty.EvKey k mods) -> case keyCmd (usPane us) k mods of
      Nothing -> continueWithoutRedraw
      Just cmd -> case step now cmd us of
        (_, EffQuit) -> halt
        (us', EffReload) -> do
          r <- liftIO (reloadJournal opts files)
          settle now (applyReload now r us')
        (us', EffNone) -> settle now us'
    VtyEvent (Vty.EvResize _ _) -> settle now us
    _ -> continueWithoutRedraw

app :: InputOpts -> [FilePath] -> ColorMode -> App AppState Tick Name
app opts files cm = App
  { appDraw = draw
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent opts files
  , appStartEvent = get >>= \(AppState us now _) -> settle now us
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
  void $ customMain vty build (Just chan) (app opts files (sColor (envSettings env))) (AppState (initState env) now Nothing)
