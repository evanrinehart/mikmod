{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.MikMod.Flags where

import Foreign

import Data.List
import Data.Maybe

import Sound.MikMod.Synonyms

#include <mikmod.h>

-- | Class to handle the bit flags. It's Enum with UWORD (CUShort) instead
-- of Int and without irrelevant functionality.
class Flag a where
  toFlag   :: a -> UWORD
  fromFlag :: UWORD -> a

data DriverModeFlag =
  DMode16Bits |
  DModeStereo |
  DModeSoftSndfx |
  DModeSoftMusic |
  DModeHQMixer |
  DModeFloat |
  DModeSurround |
  DModeInterp |
  DModeReverse |
  DModeSIMDMixer |
  DModeNoiseReduction
    deriving (Eq, Show)

instance Flag DriverModeFlag where
  toFlag flag = case flag of
    DMode16Bits -> (#const DMODE_16BITS)
    DModeStereo -> (#const DMODE_STEREO)
    DModeSoftSndfx -> (#const DMODE_SOFT_SNDFX)
    DModeSoftMusic -> (#const DMODE_SOFT_MUSIC)
    DModeHQMixer -> (#const DMODE_HQMIXER )
    DModeFloat -> (#const DMODE_FLOAT)
    DModeSurround -> (#const DMODE_SURROUND)
    DModeInterp -> (#const DMODE_INTERP)
    DModeReverse -> (#const DMODE_REVERSE)
    DModeSIMDMixer -> (#const DMODE_SIMDMIXER)
    DModeNoiseReduction -> (#const DMODE_NOISEREDUCTION)
  fromFlag n = case n of
    (#const DMODE_16BITS) -> DMode16Bits
    (#const DMODE_STEREO) -> DModeStereo
    (#const DMODE_SOFT_SNDFX) -> DModeSoftSndfx
    (#const DMODE_SOFT_MUSIC) -> DModeSoftMusic
    (#const DMODE_HQMIXER) -> DModeHQMixer
    (#const DMODE_FLOAT) -> DModeFloat
    (#const DMODE_SURROUND) -> DModeSurround
    (#const DMODE_INTERP) -> DModeInterp
    (#const DMODE_REVERSE) -> DModeReverse
    (#const DMODE_SIMDMIXER) -> DModeSIMDMixer
    (#const DMODE_NOISEREDUCTION) -> DModeNoiseReduction
    _ -> error ("unmarshalDriverModeFlag " ++ show n)

data SampleFlag =
  SF16Bits |
  SFBigEndian |
  SFDelta |
  SFITPacked |
  SFSigned |
  SFStereo |
  SFBidi |
  SFLoop |
  SFReverse
    deriving (Eq, Show)

instance Flag SampleFlag where
  toFlag flag = case flag of
    SF16Bits -> (#const SF_16BITS)
    SFBigEndian -> (#const SF_BIG_ENDIAN)
    SFDelta -> (#const SF_DELTA) 
    SFITPacked -> (#const SF_ITPACKED)
    SFSigned -> (#const SF_SIGNED)
    SFStereo -> (#const SF_STEREO)
    SFBidi -> (#const SF_BIDI)
    SFLoop -> (#const SF_LOOP)
    SFReverse -> (#const SF_REVERSE)
  fromFlag n = case n of
    (#const SF_16BITS) -> SF16Bits
    (#const SF_BIG_ENDIAN) -> SFBigEndian
    (#const SF_DELTA) -> SFDelta 
    (#const SF_ITPACKED) -> SFITPacked
    (#const SF_SIGNED) -> SFSigned
    (#const SF_STEREO) -> SFStereo
    (#const SF_BIDI) -> SFBidi
    (#const SF_LOOP) -> SFLoop
    (#const SF_REVERSE) -> SFReverse
    _ -> error ("unmarshalSampleFlag " ++ show n)

data ModuleFlag =
  UFARPMem |
  UFBGSlides |
  UFHighBPM |
  UFInst |
  UFLinear |
  UFNNA |
  UFNoWrap |
  UFS3MSlides |
  UFXMPeriods |
  UFT2Quirks |
  UFPanning
    deriving (Eq, Show)

instance Flag ModuleFlag where
  toFlag flag = case flag of
    UFARPMem -> (#const UF_ARPMEM)
    UFBGSlides -> (#const UF_BGSLIDES)
    UFHighBPM -> (#const UF_HIGHBPM)
    UFInst -> (#const UF_INST)
    UFLinear -> (#const UF_LINEAR)
    UFNNA -> (#const UF_NNA)
    UFNoWrap -> (#const UF_NOWRAP)
    UFS3MSlides -> (#const UF_S3MSLIDES)
    UFXMPeriods -> (#const UF_XMPERIODS)
    UFT2Quirks -> (#const UF_FT2QUIRKS)
    UFPanning -> (#const UF_PANNING)
  fromFlag n = case n of
    (#const UF_ARPMEM) -> UFARPMem
    (#const UF_BGSLIDES) -> UFBGSlides
    (#const UF_HIGHBPM) -> UFHighBPM
    (#const UF_INST) -> UFInst
    (#const UF_LINEAR) -> UFLinear
    (#const UF_NNA) -> UFNNA
    (#const UF_NOWRAP) -> UFNoWrap
    (#const UF_S3MSLIDES) -> UFS3MSlides
    (#const UF_XMPERIODS) -> UFXMPeriods
    (#const UF_FT2QUIRKS) -> UFT2Quirks
    (#const UF_PANNING) -> UFPanning
    _ -> error ("unmarshalModuleFlag " ++ show n)

unpackFlags :: Flag a => UWORD -> [a]
unpackFlags packed = map fromFlag results where
  experiment = map (packed .&.) (take 16 $ (iterate (*2) 1))
  results = filter (> 0) experiment

packFlags :: Flag a => [a] -> UWORD
packFlags flags = foldl' (.|.) 0 (map toFlag flags)
