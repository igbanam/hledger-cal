{-# LANGUAGE CPP #-}

-- | Windows defaults to the ANSI code page for files and the console, but
-- journals are UTF-8 and so is every glyph we draw. Elsewhere the locale
-- decides, as it does for hledger.
module Cal.Platform
  ( utf8Journals
  , utf8Console
  ) where

#if defined(mingw32_HOST_OS)
import Control.Monad (void)
import Data.Word (Word32)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
#endif

-- | Read journals as UTF-8.
utf8Journals :: IO ()

-- | Read journals as UTF-8 and have the console show UTF-8 output.
utf8Console :: IO ()

#if defined(mingw32_HOST_OS)
utf8Journals = setLocaleEncoding utf8

utf8Console = do
  utf8Journals
  void (setConsoleOutputCP 65001)
  void (setConsoleCP 65001)

foreign import ccall unsafe "windows.h SetConsoleOutputCP"
  setConsoleOutputCP :: Word32 -> IO Bool

foreign import ccall unsafe "windows.h SetConsoleCP"
  setConsoleCP :: Word32 -> IO Bool
#else
utf8Journals = pure ()

utf8Console = pure ()
#endif
