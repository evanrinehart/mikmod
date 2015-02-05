module Sound.MikMod.Types where

import Foreign.Ptr
import Foreign.C.Types
import System.IO
import Data.Word (Word8)
import Data.ByteString (ByteString)

import Sound.MikMod.Synonyms
import Sound.MikMod.Flags

-- | MikMod distinguishes module channels from voices. Sound effects and music
-- both work by playing samples on voices. At most one sample can play on a
-- voice at a time. Operations on the voice level take a voice number which you
-- can get using 'Sound.MikMod.playerGetChannelVoice' and 'Sound.MikMod.samplePlay'.
newtype Voice = Voice { marshalVoice :: SBYTE }
  deriving (Eq, Ord, Show)

-- | Inclusive or exclusive selection of channels for muting.
data MuteOperation = MuteInclusive | MuteExclusive
  deriving (Eq, Show)

-- | When loading a module, Curious will cause the loader to attempt to load
-- hidden tracks past the end of the song.
data CuriousFlag = Curious | NotCurious
  deriving (Eq, Show)

-- | Pan settings.
data Pan = Pan Int | PanSurround deriving (Eq, Show)

data MDriver

data MDriverInfo = MDriverInfo
  { mdriverName :: String
  , mdriverHardVoiceLimit :: Int
  , mdriverSoftVoiceLimit :: Int
  , mdriverAlias :: String
  } deriving (Show)

data Module

-- | Handle to a Module object which contains the music data and current
-- playback state of a song.
type ModuleHandle = Ptr Module

-- | Static info about a module.
data ModuleInfo = ModuleInfo
  { moduleSongname       :: String
  , moduleModType        :: String
  , moduleComment        :: Maybe String
  , moduleFlags          :: [ModuleFlag]
  , moduleNumChannels    :: Int
  , moduleNumVoices      :: Int
  , moduleNumPositions   :: Int
  , moduleNumPatterns    :: Int
  , moduleNumInstruments :: Int
  , moduleNumSamples     :: Int
  , moduleInstruments    :: Maybe [String]
  } deriving (Show)

data Sample

type SampleHandle = Ptr Sample

-- | Static info about a sample.
data SampleInfo = SampleInfo
  { samplePanning   :: Pan
  , sampleSpeed     :: Int
  , sampleVolume    :: Int
  , sampleFlags     :: [SampleFlag]
  , sampleInflags   :: [SampleFlag]
  , sampleLength    :: Int
  , sampleLoopStart :: Int
  , sampleLoopEnd   :: Int
  } deriving (Show)

-- | Collection of IO operations that MikMod can use to load data from an
-- arbitrary source, such as a memory buffer or zip file.
data MReader = MReader
  { -- | Move the read position. Return Ok for success or Fail for failure.
    readerSeek :: Int -> SeekMode -> IO Outcome
    -- | Report the current read position.
  , readerTell :: IO Int
    -- | Return a ByteString of length (at most) n and advance the read position.
    -- Return an empty ByteString if already at EOF.
    -- Return Nothing in case of an error.
  , readerRead :: Int -> IO (Maybe ByteString)
    -- | Return one byte and advance the read position. If an error occurs or
    -- we are at the end-of-stream, then return Nothing.
  , readerGet  :: IO (Maybe Word8)
    -- | Return EOF if we are at the end of the stream. Otherwise return NotEOF.
  , readerEof  :: IO IsEOF }

-- | Used for the very undescriptive possible outcomes of a readerSeek.
data Outcome = Ok | Fail deriving (Eq, Show)

-- | The result of a readerEof call.
data IsEOF = EOF | NotEOF deriving (Eq, Show)

data Instrument
