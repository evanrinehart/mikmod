module Sound.MikMod.Types where

import Foreign.Ptr
import Foreign.C.Types

import Sound.MikMod.Synonyms
import Sound.MikMod.Flags

-- | MikMod distinguishes module channels from voices. Sound effects and music
-- both work by playing samples on voices. At most one sample can play on a
-- voice at a time. Operations on the voice level take a voice number which you
-- can get using mikmodGetChannelVoice.
newtype Voice = Voice { marshalVoice :: SBYTE }
  deriving (Eq, Ord, Show)

-- | Operations that manipulate muting of multiple channels use one of two
-- interpretations of the specified channge range. MuteExclusive means mute /
-- unmute / toggle muting of all channels outside the specified range.
data MuteOperation = MuteInclusive | MuteExclusive
  deriving (Eq, Ord, Show)

-- | MDriver is an opaque type that represents a audio driver for audio output.
-- After using mikmodRegisterAllDrivers use mikmodInfoDriver to get a listing
-- of all drivers MikMod has available for your system.
data MDriver

-- | The only thing you can do with a MDriverHandle is peekMDriver to get
-- some basic information provided by MikMod. MDrivers are larely for internal
-- library use only.
type MDriverHandle = Ptr MDriver

-- | Read-only limited view of an MDriver.
data MDriverInfo = MDriverInfo
  { mdriverName :: String
  , mdriverHardVoiceLimit :: Int
  , mdriverSoftVoiceLimit :: Int
  , mdriverAlias :: String
  } deriving (Show)

-- | A Module is an opaque type representing a piece of tracker music.
-- Load modules from the file system with 'playerLoad'.
data Module

-- | MikMod exposes control over how a module is played by expecting the
-- programmer to poke some (but only some) fields in this structure. Use
-- 'peekModule' to get a read-only view the module's fields. Use pokeModule
-- functions to manipulate particular fields relevant to module playback.
-- Manipulating fields of a module has a real-time effect on a playing module.
type ModuleHandle = Ptr Module

-- | Read-only limited view of a Module.
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

-- | Sample is an opaque type for an array of audio data.
data Sample

-- | MikMod exposes control over playback of samples by expecting the programmer
-- to manipulate certain fields (but only certain fields) in the Sample structure.
-- Use 'peekSample' to get a read-only view of the sample structure. Use one of
-- the pokeSample operations to manipulate the fields relevant to sample playback.
-- Manipulating a sample changes its characteristics on all voices in real time.
type SampleHandle = Ptr Sample

-- | Read-only view of a sample structure.
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

