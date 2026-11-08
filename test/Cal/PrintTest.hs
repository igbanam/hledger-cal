module Cal.PrintTest where

import Control.Monad (forM, forM_, unless)
import Data.Char (GeneralCategory (..), generalCategory, isDigit)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.Directory (doesFileExist, findExecutable, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcess, readProcessWithExitCode)
import Test.Tasty.HUnit

import Cal.Doc (textW)
import Cal.Print (renderPrint)
import Support (designPath, edgePath, envFor)

unit_tooNarrow :: Assertion
unit_tooNarrow = do
  e <- envFor ["-M", "-p", "2026-10"]
  renderPrint e 79 @?= Left "hledger cal needs at least 80 columns (have 79)"

faces :: [[String]]
faces = [["-Y"], ["-Q"], ["-M"], ["-W"], ["--commodities"]]

unit_allWidths :: Assertion
unit_allWidths =
  forM_ faces $ \face -> forM_ [designPath, edgePath] $ \path -> forM_ [80, 95, 100, 119, 120, 139, 140, 200] $ \w -> do
    e <- envFor (face ++ ["-f", path, "--color=never"])
    case renderPrint e w of
      Left err -> assertFailure (show (face, path, w) ++ ": " ++ T.unpack err)
      Right out -> forM_ (T.lines out) $ \l ->
        assertBool (show (face, path, w) ++ " too wide: " ++ T.unpack l) (textW l <= w)

unit_narrowKeepsFigures :: Assertion
unit_narrowKeepsFigures = do
  e <- envFor ["-M", "-p", "2026-10", "--color=never"]
  out <- either (assertFailure . T.unpack) pure (renderPrint e 80)
  forM_ ["+2,680,000", "\x2212\&2,305,000", "11 txns"] $ \needle ->
    assertBool ("missing " ++ T.unpack needle) (needle `T.isInfixOf` out)

findBin :: IO FilePath
findBin = do
  envCabal <- lookupEnv "CABAL"
  onPath <- findExecutable "cabal"
  home <- getHomeDirectory
  let fallback = home ++ "/.ghcup/bin/cabal"
  fallbackOk <- doesFileExist fallback
  cabal <- case (envCabal, onPath) of
    (Just c, _) -> pure c
    (_, Just c) -> pure c
    _ | fallbackOk -> pure fallback
    _ -> assertFailure "cabal not found (set CABAL)"
  out <- readProcess cabal ["list-bin", "hledger-cal"] ""
  let bin = T.unpack (T.strip (T.pack out))
  ok <- doesFileExist bin
  unless ok (assertFailure ("executable not built: " ++ bin))
  pure bin

unit_mainPrint :: Assertion
unit_mainPrint = do
  bin <- findBin
  (code, out, err) <- readProcessWithExitCode bin
    ["--print", "--width", "140", "-M", "-p", "2026-10", "-f", designPath, "--color=never"] ""
  err @?= ""
  code @?= ExitSuccess
  golden <- TE.decodeUtf8 <$> BS.readFile "test/golden/month-2026-10-140.txt"
  T.pack out @?= golden

unit_mainBadJournal :: Assertion
unit_mainBadJournal = do
  bin <- findBin
  (code, _, err) <- readProcessWithExitCode bin ["-f", "test/fixtures/nope.journal", "--print"] ""
  code @?= ExitFailure 1
  assertBool "stderr non-empty" (not (null err))

-- | No golden may show a cut figure: an ellipsis straight after a digit, a
-- digit-group comma or a currency symbol.
unit_goldensHaveNoCutFigures :: Assertion
unit_goldensHaveNoCutFigures = do
  files <- filter (".txt" `isSuffixOf`) <$> listDirectory "test/golden"
  assertBool "goldens found" (not (null files))
  found <- fmap concat $ forM files $ \name -> do
    t <- TE.decodeUtf8 <$> BS.readFile ("test/golden/" ++ name)
    pure [ name ++ ":" ++ show i ++ ": figure cut after " ++ T.unpack c
         | (i, l) <- zip [1 :: Int ..] (T.lines t), c <- cutsIn l ]
  unless (null found) (assertFailure (unlines found))
  where
    cutsIn l = [ T.takeEnd 12 before | ell <- ["…", "..."], (before, _) <- T.breakOnAll ell l
                                     , Just (_, c) <- [T.unsnoc before], isFigureChar c ]
    isFigureChar c = isDigit c || c == ',' || generalCategory c == CurrencySymbol
