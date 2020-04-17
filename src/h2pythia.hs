{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import HEP.Data.THDM.Coupling (coupH2, coupHp)
import HEP.Data.THDM.Width    (printWidthH2, printWidthHp)

import HEP.Data.Kinematics    (Mass (..))
import HEP.Data.THDM
import HEP.Data.Util          (mkAngles)

import Options.Generic

import Control.Monad          (when)
import Data.Maybe             (fromMaybe)
import System.Exit            (die)
import System.IO              (IOMode (..), hPutStrLn, withFile)

main :: IO ()
main = do
    inp <- unwrapRecord $
           "Generate the input card file to run Pythia 8" <>
           " for the heavy Higgs process in the 2HDM"

    let h2decaysExec = h2decays inp
        hpdecaysExec = hpdecays inp
    putStrLn $ "-- We use h2decays from " ++ h2decaysExec
        ++ ", and hpdecays from " ++ hpdecaysExec

    let mdtypVal = fromIntToType $ fromMaybe 2 (mtype inp)
    when (mdtypVal == UnknownType) $ die "-- The type must be either 1 or 2."

    let sqrtS = fromMaybe 13000 (eCM inp)
        (tanbVal, cosbaVal) = (,) <$> tanb <*> cosba $ inp
        (mHVal, mHpVal) = (,) <$> mH <*> mHp $ inp
        mAVal = fromMaybe mHpVal (mA inp)
        m12Val = fromMaybe (defaultM12 tanbVal mHVal) (m12 inp)

    putStrLn $ "-- m_{H} = " ++ show mHVal
        ++ ", m_{H+} = " ++ show mHpVal
        ++ ", tan(beta) = " ++ show tanbVal
        ++ ", cos(beta - alpha) = " ++ show cosbaVal

    let param = InputParam { _mdtyp = mdtypVal
                           , _mH    = Mass mHVal
                           , _mA    = Mass mAVal
                           , _mHp   = Mass mHpVal
                           , _m12   = Mass m12Val
                           , _angs  = mkAngles tanbVal cosbaVal
                           }

    let outfile = fromMaybe "pythia8.cmnd" (card inp)
    withFile outfile WriteMode $ \h -> do
        hPutStrLn h $ "Beams:eCM = " <> show sqrtS <> "\n"
        hPutStrLn h "Higgs:useBSM = on\n"

        hPutStrLn h $ "! the H2 couplings\n"
            <> "! See http://home.thep.lu.se/~torbjorn/pythia82html/HiggsProcesses.html"
        hPutStrLn h $ coupH2 param

        hPutStrLn h "! the H+ couplings"
        hPutStrLn h $ coupHp param

        hPutStrLn h "! H2"
        hPutStrLn h $ "35:m0 = " <> show mHVal
        printWidthH2 h h2decaysExec param
        hPutStrLn h "35:doForceWidth = on\n"

        hPutStrLn h "! A"
        hPutStrLn h $ "36:m0 = " <> show mAVal

        hPutStrLn h "\n! H+"
        hPutStrLn h $ "37:m0 = " <> show mHpVal
        printWidthHp h hpdecaysExec param
        hPutStrLn h "37:doForceWidth = on"

    putStrLn $ "-- " ++ outfile ++ " has been generated."

data InputArgs w = InputArgs
    { h2decays :: w ::: FilePath     <?> "the executable path of h2decays"
    , hpdecays :: w ::: FilePath     <?> "the executable path of hpdecays"
    , eCM      :: w ::: Maybe Double <?>
        "center-of-mass energy in GeV (default: 13000 GeV)"
    , mtype    :: w ::: Maybe Int    <?> "model type (either 1 or 2)"
    , mH       :: w ::: Double       <?> "heavy Higgs mass"
    , mA       :: w ::: Maybe Double <?> "CP-odd Higgs mass"
    , mHp      :: w ::: Double       <?> "charged Higgs mass"
    , m12      :: w ::: Maybe Double <?> "soft Z2 breaking term"
    , tanb     :: w ::: Double       <?> "tan(beta)"
    , cosba    :: w ::: Double       <?> "cos(beta-alpha)"
    , card     :: w ::: Maybe String <?> "the name of the input card file"
    } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)
