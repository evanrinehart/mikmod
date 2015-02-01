module Sound.MikMod.Types where

import Foreign.Ptr
import Foreign.C.Types

type UBYTE = CUChar
type SBYTE = CChar
type UWORD = CShort
type SWORD = CUShort
type ULONG = CUInt
type SLONG = CInt
type BOOL = CInt

newtype Voice = Voice { marshalVoice :: SBYTE }
  deriving (Eq, Ord, Show)

data MuteOperation = MuteInclusive | MuteExclusive
  deriving (Eq, Ord, Show)

data MDriver

data MDriverInfo = MDriverInfo
  { mdriverName :: String
  , mdriverHardVoiceLimit :: Int
  , mdriverSoftVoiceLimit :: Int
  , mdriverAlias :: String
  } deriving (Show)
type MDriverHandle = Ptr MDriver

data Module

data ModuleInfo = ModuleInfo
  { moduleSongname :: String
  , moduleModType :: String
{-  , moduleComment :: String
  , moduleFlags :: [ModuleFlag]
  , moduleInstruments :: [String]
  , moduleSamples :: [Ptr Sample]
  , moduleBPM :: Int
-}
  }
type ModuleHandle = Ptr Module

newtype ModuleFlag = ModuleFlag { marshalModuleFlag :: UWORD }
  deriving (Eq, Show)


data Sample
data SampleInfo = SampleInfo
  { samplePanning :: Int
  , sampleSpeed :: Int
  , sampleVolume :: Int
  , sampleFlags :: [SampleFlag]
  , sampleInflags :: [SampleFlag]
  , sampleLength :: Int
  , sampleLoopStart :: Int
  , sampleLoopEnd :: Int
  } deriving (Show)
type SampleHandle = Ptr Sample


newtype SampleFlag = SampleFlag { marshalSampleFlag :: CUShort }
  deriving (Eq, Show)

newtype DriverModeFlag = DriverModeFlag CInt deriving (Eq, Ord, Show)
