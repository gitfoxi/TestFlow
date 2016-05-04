
-- A TestFlow may be interpreted to do any of
--
--   1. Generate a 93k testflow
--   2. Actually drive the tester / handler
--   3. Draw a flowchart
--   4. Test invariants (check consistent with specification)
--   5. Generate reports
--   6. Dump binning table
--   7. Check coverage (i.e. every pin covered by leakage)
--   8. Ask which fuses it will burn under a given condition

-- Fundamental units are A and V

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Text

data ProdStage = FT | FQA
  deriving (Eq, Show)

data Good = Good | Bad
  deriving (Eq, Show)

data Pass = Pass | Fail
  deriving (Eq, Show)

ft = (== FT)
fqa = (==FQA)

data Bin =
  Bin
  { binGood :: Good
  , binHard :: Int
  , binSoft :: Int
  , binDesc :: Text
  }

-- Problem: What if I want to add another feature to determine binning?
--          Expression problem.
-- Solution: Use HList
data BinSpec =
  BinSpec
    { iddqMaxA :: Double
    , freqMinMHz :: Double
    , bin :: Bin
    }

data ProdStageBinning =
  ProdStageBinning
    { prodStage :: ProdStage
    , binSpecs  :: [BinSpec]
    }

ftBinning =
  ProdStageBinning FT
    [ BinSpec 24.0 1600.0 binFt1
    , BinSpec 21.0 1700.0 binFt2
    ]

-- Problem: we can do a runtime check to ensure good bins match bins marked
-- good in the spac and bad bins match bad ones; compile-time check would be
-- better
-- Solution: Dependant types (Peano numbers)
goodHardBins = [1..4]
badHardBins = [5]

binFt1 = Bin Good 1 1      "FT Iddq < 24A Fmax > 1600MHz" -- binDesc could be derived from binSpec so bad redundancy
binFt2 = Bin Good 2 2      "FT Iddq < 21A Fmax > 1700MHz"
binFtFail = Bin Bad 5 999 "Can't satisfy any good bin"

mega = (* 1000000)

prodstage :: M ProdStage
prodstage = return FT -- dummy for testing

{-
binOut :: ProdStage -> Iddq -> Fmax -> Bin
binOut FT = BinSpec ftBinning
-}



type M = IO -- my monad for now

runContinuity :: M Pass
runContinuity = return Pass

runIddq :: M Double
runIddq = return 12.5

newtype MHz =  MHz Double
  deriving (Eq, Show, Num)

runFmax :: M MHz
runFmax = return (MHz 1650.0)

getProdStage = return FT

type Iddq = Double
type Fmax = Double

main = do

  -- External Inputs:
  --
  --   prodStage - from application model or entered by operator another way

  prodStage <- getProdStage
  continuity <- runContinuity -- TODO: stop running on fail and also support overon; need a monad runner
  iddq <- runIddq -- TODO: rather than explicit order, support implicit -- directed by the binnig table
  fmax <- runFmax

  let
    bin :: ProdStage -> Iddq -> Fmax -> Bin
    bin FT iddq fmax
      | iddq < 24 && fmax > mega 1600 = binFt1

  return ()
