module Cal.JournalTest where

import Cal.Types
import Test.Tasty.HUnit

unit_settingsTypesCompile :: Assertion
unit_settingsTypesCompile = defaultCursor @?= Cursor 0 0
