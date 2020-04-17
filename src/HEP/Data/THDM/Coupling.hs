{-# LANGUAGE RecordWildCards #-}

module HEP.Data.THDM.Coupling (coupH2, coupHp) where

import HEP.Data.Constants  (gW, mW, mZ)
import HEP.Data.Kinematics (Mass (..), massSq)
import HEP.Data.THDM
import HEP.Data.Util       (cosBetaAlpha, sinBetaAlpha, tanBeta)

coupH2 :: InputParam -> String
coupH2 inp =
    c2d <> c2u <> c2l <> c2Z <> c2W <> c2Hchg <> c2H1H1 <> c2A3A3 <> c2HchgW
  where
    c2d     = "HiggsH2:coup2d = "     <> show (coup2d     inp) <> "\n"
    c2u     = "HiggsH2:coup2u = "     <> show (coup2u     inp) <> "\n"
    c2l     = "HiggsH2:coup2l = "     <> show (coup2l     inp) <> "\n"
    c2Z     = "HiggsH2:coup2Z = "     <> show (coup2Z     inp) <> "\n"
    c2W     = "HiggsH2:coup2W = "     <> show (coup2W     inp) <> "\n"
    c2Hchg  = "HiggsH2:coup2Hchg = "  <> show (coup2Hchg  inp) <> "\n"
    c2H1H1  = "HiggsH2:coup2H1H1 = "  <> show (coup2H1H1  inp) <> "\n"
    c2A3A3  = "HiggsH2:coup2A3A3 = "  <> show (coup2A3A3  inp) <> "\n"
    c2HchgW = "HiggsH2:coup2HchgW = " <> show (coup2HchgW inp) <> "\n"

coup2d :: InputParam -> Double
coup2d InputParam {..} | _mdtyp == TypeI  = cosba - sinba / tanb
                       | _mdtyp == TypeII = cosba + sinba * tanb
                       | otherwise        = 0
  where
    cosba = cosBetaAlpha _angs
    tanb  = tanBeta      _angs
    sinba = sinBetaAlpha _angs

coup2u :: InputParam -> Double
coup2u inp = coup2d (inp { _mdtyp = TypeI })

coup2l :: InputParam -> Double
coup2l = coup2d

coup2W :: InputParam -> Double
coup2W InputParam {..} = cosBetaAlpha _angs

coup2Z :: InputParam -> Double
coup2Z = coup2W

coup2Hchg :: InputParam -> Double
coup2Hchg InputParam {..} = (/ g) $ gHHpHm _mH _mHp _m12 _angs
  where g = gW * getMass mW

coup2H1H1 :: InputParam -> Double
coup2H1H1 InputParam {..} = (/ g) $ gHhh _mH _m12 _angs
  where g = gW * massSq mZ / (2 * getMass mW)

coup2A3A3 :: InputParam -> Double
coup2A3A3 InputParam {..} = (/ g) $ gHAA _mH _mA _m12 _angs
  where g = gW * massSq mZ / (2 * getMass mW)

coup2HchgW :: InputParam -> Double
coup2HchgW InputParam {..} = sinBetaAlpha _angs

coupHp :: InputParam -> String
coupHp InputParam {..} = tanb <> cH1W <> cH2W
  where
    tanb = "HiggsHchg:tanBeta = "  <> show (tanBeta      _angs) <> "\n"
    cH1W = "HiggsHchg:coup2H1W = " <> show (cosBetaAlpha _angs) <> "\n"
    cH2W = "HiggsHchg:coup2H2W = " <> show (sinBetaAlpha _angs) <> "\n"
