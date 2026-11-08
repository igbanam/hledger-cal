module Main (main) where

import Cal.Cli (loadInputOpts, mkSettings, parseCli)
import Cal.Journal (loadJournal)
import Cal.Model.Common (mkEnv)
import Cal.Print (renderPrint)
import Cal.Tui.App (runTui)
import Cal.Types (Output (..), Settings (..), Terminal (..))
import Data.Char (toLower)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger.Cli (CliOpts (..))
import Hledger.Read.InputOptions (InputOpts)
import Hledger.Reports.ReportOptions (ReportSpec (..))
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitWith, ExitCode (..))
import System.IO (hIsTerminalDevice, hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import GHC.IO.Encoding (getLocaleEncoding, textEncodingName)
import Text.Read (readMaybe)

main :: IO ()
main = do
  opts <- getArgs >>= parseCli
  -- today, or hledger's --today
  let today = _rsDay (reportspec_ opts)
      iopts = loadInputOpts today opts :: InputOpts
  loaded <- loadJournal iopts (file_ opts)
  j <- either die' pure loaded
  tty <- hIsTerminalDevice stdout
  enc <- getLocaleEncoding
  cols <- (>>= (\c -> readMaybe c >>= \n -> if n > 0 then Just n else Nothing)) <$> lookupEnv "COLUMNS"
  noColor <- (/= Nothing) <$> lookupEnv "NO_COLOR"
  ct <- lookupEnv "COLORTERM"
  let term = Terminal
        { tIsTty = tty
        , tUtf8 = "utf-8" `isInfixOf` map toLower (textEncodingName enc)
        , tColumns = cols
        , tNoColorEnv = noColor
        , tTrueColor = ct `elem` [Just "truecolor", Just "24bit"] }
  s <- either die' pure (mkSettings today term opts j)
  let env = mkEnv j s
      printAt w = case renderPrint env w of
        Left err -> die' (T.unpack err)
        Right out -> hSetEncoding stdout utf8 >> T.putStrLn out
  case sOutput s of
    OutPrint w -> printAt w
    OutTui -> runTui iopts (file_ opts) env
  where
    die' err = hSetEncoding stderr utf8 >> hPutStrLn stderr err >> exitWith (ExitFailure 1)
