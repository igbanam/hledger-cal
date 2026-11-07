module Cal.JournalTest where

import Cal.Journal
import Cal.Types
import Data.Either (isLeft)
import Data.Time.Calendar (fromGregorian)
import Hledger (definputopts, jtxns, nullposting, Posting (..), tdescription, tpostings)
import Support
import Test.Tasty.HUnit

unit_settingsTypesCompile :: Assertion
unit_settingsTypesCompile = defaultCursor @?= Cursor 0 0

unit_designLoads :: Assertion
unit_designLoads = do j <- loadFixture designPath; length (jtxns j) @?= 55

unit_kinds :: Assertion
unit_kinds = do
  j <- loadFixture designPath
  let k a = postingKind j nullposting{paccount = a}
  (k "income:salary", k "expenses:fuel", k "assets:bank:ngn") @?= (Just KIncome, Just KExpense, Nothing)

unit_kindsInferredFromNames :: Assertion
unit_kindsInferredFromNames = do
  j <- loadFixture edgePath
  postingKind j nullposting{paccount = "expenses:food"} @?= Just KExpense

unit_locationPostingWins :: Assertion
unit_locationPostingWins = do
  j <- loadFixture edgePath
  let ps = [p | t <- jtxns j, tdescription t == "Refund", p <- tpostings t, paccount p == "expenses:food"]
  map postingLocation ps @?= [Just "Ikeja"]

unit_dataRange :: Assertion
unit_dataRange = do j <- loadFixture designPath; journalDataRange fixtureToday j @?= (fromGregorian 2026 1 1, fixtureToday)

unit_firstSeen :: Assertion
unit_firstSeen = do j <- loadFixture edgePath; firstSeenCommodity j @?= Just "₦"

unit_missingFileIsLeft :: Assertion
unit_missingFileIsLeft = do r <- loadJournal definputopts ["test/fixtures/nope.journal"]; assertBool "Left" (isLeft r)
