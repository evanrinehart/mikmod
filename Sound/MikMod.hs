-- |
-- Module : Sound.MikMod
-- License : LGPL3
-- 
-- MikMod bindings for Haskell

module Sound.MikMod 
(
  -- * Overview
  -- $overview

  -- * Quickstart
  -- $quickstart

  -- * Globals
  mikmodSetMusicVolume,
  mikmodGetMusicVolume,
  mikmodSetPanSep,
  mikmodGetPanSep,
  mikmodSetReverb,
  mikmodGetReverb,
  mikmodSetSndFXVolume,
  mikmodGetSndFXVolume,
  mikmodSetVolume,
  mikmodGetVolume,
  mikmodSetDeviceIndex,
  mikmodGetDeviceIndex,
  MDriverInfo(..),
  mikmodGetDriver,
  mikmodGetMixFreq,
  mikmodSetMixFreq,
  DriverModeFlag(..),
  mikmodModifyDriverModeFlags,
  mikmodGetDriverModeFlags,
  mikmodSetDriverModeFlags,

  -- * Core Operations
  runMikMod,
  mikmodSetup,
  mikmodGetVersion,
  mikmodGetError,
  mikmodRegisterAllDrivers,
  mikmodRegisterAllLoaders,
  mikmodInit,
  mikmodInitSafe,
  mikmodActive,
  mikmodInfoDriver,
  mikmodInfoLoader,
  mikmodSetNumVoices,
  mikmodSetNumVoicesSafe,
  mikmodReset,
  mikmodResetSafe,
  mikmodDisableOutput,
  mikmodEnableOutput,
  mikmodUpdate,
  mikmodExit,

  -- * Module Player Operations
  CuriousFlag(..),
  playerLoad,
  playerLoadSafe,
  playerLoadGeneric,
  playerLoadGenericSafe,
  playerLoadTitle,
  playerStart,
  playerStop,
  playerPaused,
  playerTogglePause,
  playerActive,
  playerFree,
  playerGetChannelVoice,
  playerGetModule,
  MuteOperation(..),
  playerMuteChannel,
  playerMuteChannels,
  playerUnmuteChannel,
  playerUnmuteChannels,
  playerToggleMuteChannel,
  playerToggleMuteChannels,
  playerMuted,
  playerNextPosition,
  playerPrevPosition,
  playerSetPosition,
  playerSetSpeed,
  playerSetTempo,

  -- * Module Operations
  ModuleHandle,
  ModuleInfo(..),
  ModuleFlag(..),
  getModuleInfo,
  getModuleRealChannels,
  getModuleTotalChannels,
  getModuleSongTime,
  getModuleSongPosition,
  getModulePatternPosition,
  setModuleInitSpeed,
  getModuleInitSpeed,
  setModuleInitTempo,
  getModuleInitTempo,
  setModulePanning,
  getModulePanning,
  setModuleChannelVolume,
  getModuleChannelVolume,
  setModuleBPM,
  getModuleBPM,
  setModuleSongSpeed,
  getModuleSongSpeed,
  setModuleExtSpeed,
  getModuleExtSpeed,
  setModulePanFlag,
  getModulePanFlag,
  setModuleWrap,
  getModuleWrap,
  setModuleRepeatPosition,
  getModuleRepeatPosition,
  setModuleLoop,
  getModuleLoop,
  setModuleFadeout,
  getModuleFadeout,
  setModuleRelativeSpeed,
  getModuleRelativeSpeed,
  getModuleSamples,

  -- * Sample Operations
  SampleHandle,
  SampleInfo(..),
  sampleLoad,
  sampleLoadSafe,
  sampleLoadGeneric,
  sampleLoadGenericSafe,
  samplePlay,
  samplePlayCritical,
  sampleFree,
  getSampleInfo,
  Pan(..),
  panLeft,
  panRight,
  setSamplePanning,
  getSamplePanning,
  setSampleSpeed,
  getSampleSpeed,
  setSampleVolume,
  getSampleVolume,
  SampleFlag(..),
  modifySampleFlags,
  getSampleFlags,
  setSampleFlags,
  getSampleInFlags,
  getSampleLength,
  setSampleLoopStart,
  getSampleLoopStart,
  setSampleLoopEnd,
  getSampleLoopEnd,

  -- * Voice Operations
  Voice(..),
  voicePlay,
  voiceStop,
  voiceStopped,
  voiceSetVolume,
  voiceGetVolume,
  voiceSetFrequency,
  voiceGetFrequency,
  voiceSetPanning,
  voiceGetPanning,
  voiceGetPosition,
  voiceRealVolume,

  -- * MReaders
  MReader(..),
  Outcome(..),
  IsEOF(..),
  byteStringReader,
  handleReader,

  -- * Errors
  MikModError(..),
  MikModException(..),
  describeMikModError,
  getErrno,
  MikModErrno(..),

  -- * Esoterica
  mikmodInitThreads,
  withMikMod

)
where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.String
import Data.Functor
import Control.Applicative
import Control.Exception
import Data.Bits

import Sound.MikMod.Synonyms
import Sound.MikMod.Types
import Sound.MikMod.Errors
import Sound.MikMod.Flags
import Sound.MikMod.Internal
import Sound.MikMod.MReader
import Sound.MikMod.Module
import Sound.MikMod.Sample

-- $overview
--
-- <http://mikmod.sourceforge.net/ MikMod> is a C library for 
-- playing music modules and sound samples.
-- 
-- The user controls MikMod by manipulating a handful of global variables,
-- calling API functions, and manipulating fields of the Module and Sample
-- structure. These low-level bindings are basically convenience wrappers for
-- the above operations.
--
-- Module objects represent not only music but the playback state of a song.
-- In this sense you can think of Modules as being like cassette tapes. For
-- example if a playing module is paused or stopped, and another module begins
-- playing, then resuming the original module will start from the position it
-- was stopped at. You manipulate modules only via the type ModuleHandle.
--
-- Sample objects represent single sounds. Modules use samples to make music,
-- but you can use samples independently for sound effects. Samples are similar
-- to Modules in that they are only accessed via the type SampleHandle.
--
-- Music and sound effects both play samples on voices. There can be at most
-- one sample playing on a voice at a time. Voices can be individually adjusted
-- to change the characteristics of the samples that play on them. Voices are
-- exposed by MikMod as indexes. These indexes are wrapped in the Voice newtype.
-- 
-- Modifying values of global variables and structure fields is allowed during
-- playback and in most cases will have immediate effect.
--
-- MikMod allows loading modules and samples from the file system or from an
-- arbitrary source via the MReader structure.
--
-- API functions that may fail come in two flavors: one that throws an exception
-- and one that returns an Either.
--
-- The MikMod error callback mechanism and the MWriter are not supported yet.

-- $quickstart
--
-- @
-- import Control.Concurrent (threadDelay)
-- import Control.Monad.Loops (whileM_)
-- import Sound.MikMod
--
-- main = do
--   mikmodRegisterAllDrivers
--   mikmodRegisterAllLoaders
--   mikmodInit ""
--   mod <- playerLoad "rock on.mod" 128 NotCurious
--   playerStart mod
--   whileM_ playerActive $ do
--     mikmodUpdate -- might be unnecessary on your system
--     threadDelay 10000
--   playerFree mod
--   mikmodExit
-- @
--
-- Make sure to link your program to MikMod with -lmikmod. GHCI can be used to
-- experiment by using ghci -lmikmod.
--
-- Example of playing sound effects.
--
-- @
-- import Control.Concurrent (threadDelay)
-- import Control.Monad.Loops (whileM_)
-- import Data.Functor ((\<$\>))
-- import Sound.MikMod
--
-- main = do
--   mikmodRegisterAllDrivers
--   mikmodRegisterAllLoaders
--   mikmodInit ""
--   mikmodSetNumVoices (-1) 4
--   mikmodEnableOutput
--   samp <- sampleLoad "wilhelm.wav"
--   Just voice <- samplePlay samp 0
--   whileM_ (not \<$\> voiceStopped voice) $ do
--     mikmodUpdate -- might be unnecessary on your system
--     threadDelay 10000
--   sampleFree samp
--   mikmodExit
-- @
-- 
-- Or using convenience wrappers for the initialization sequence,
--
-- @
-- import Control.Concurrent (threadDelay)
-- import Control.Monad.Loops (whileM_)
-- import Data.Functor ((\<$\>))
-- import Sound.MikMod
--
-- main = runMikMod 4 $ do
--   samp <- sampleLoad "wilhelm.wav"
--   Just voice <- samplePlay samp 0
--   whileM_ (not \<$\> voiceStopped voice) $ do
--     mikmodUpdate
--     threadDelay 10000
--   sampleFree samp
-- @

-- | Set the global music volume. The argument must be in the range 0 to 128
-- (There are 129 volume levels).
mikmodSetMusicVolume :: Int -> IO ()
mikmodSetMusicVolume v = poke c_md_musicvolume (fromIntegral v)

mikmodGetMusicVolume :: IO Int
mikmodGetMusicVolume = fromIntegral <$> peek c_md_musicvolume

-- | Set the global stereo separation. The argument must be in the range 0 to
-- 128 where 0 means mono sound and 128 means full separation. The default
-- pan sep is 128.
mikmodSetPanSep :: Int -> IO ()
mikmodSetPanSep v = poke c_md_pansep (fromIntegral v)

mikmodGetPanSep :: IO Int
mikmodGetPanSep = fromIntegral <$> peek c_md_pansep

-- | Set the global reverb. The argument must be in the range 0 to 15 where 0
-- means no reverb and 15 means extreme reverb. The default reverb is zero.
mikmodSetReverb :: Int -> IO ()
mikmodSetReverb v = poke c_md_reverb (fromIntegral v)

mikmodGetReverb :: IO Int
mikmodGetReverb = fromIntegral <$> peek c_md_reverb

-- | Set the global sound effects volume. The argument must be in the range 0 to 128.
mikmodSetSndFXVolume :: Int -> IO ()
mikmodSetSndFXVolume v = poke c_md_sndfxvolume (fromIntegral v)

mikmodGetSndFXVolume :: IO Int
mikmodGetSndFXVolume = fromIntegral <$> peek c_md_sndfxvolume


-- | Set the global overall sound volume. The argument must be in the range 0 to 128.
mikmodSetVolume :: Int -> IO ()
mikmodSetVolume v = poke c_md_volume (fromIntegral v)

mikmodGetVolume :: IO Int
mikmodGetVolume = fromIntegral <$> peek c_md_volume


-- | The selected output driver from the global 1-based list of drivers.
mikmodGetDeviceIndex :: IO Int
mikmodGetDeviceIndex = fromIntegral <$> peek c_md_device

-- | Change the selected output driver by specifying a 1-based index into the
-- global list of drivers. Setting this to zero, the default, means autodetect.
-- To see the list use 'mikmodInfoDriver'.
mikmodSetDeviceIndex :: Int -> IO ()
mikmodSetDeviceIndex i = poke c_md_device (fromIntegral i)

-- | Get an info report of the sound driver currently in use, if any. MikMod
-- does not expose any functionality via MDriver field manipulation.
mikmodGetDriver :: IO (Maybe MDriverInfo)
mikmodGetDriver = do
  r <- peek c_md_driver
  if r == nullPtr
    then return Nothing
    else Just <$> peekMDriver r

-- | Set the mix frequency measured in Hertz. Higher values mean more sound
-- quality and more CPU usage. Common values are 8000, 11025, 22100, and
-- 44100. The default is 44100.
mikmodSetMixFreq :: Int -> IO ()
mikmodSetMixFreq freq = poke c_md_mixfreq (fromIntegral freq)

mikmodGetMixFreq :: IO Int
mikmodGetMixFreq = fromIntegral <$> peek c_md_mixfreq

-- | Modify the "mode flags". These flags affect sound output in various ways.
-- For a full explanation of each one see the MikMod docs. Changing DModeInterp,
-- DModeReverse, or DModeSurround will affect playing immediately. The other
-- flags will require a reset. The default flags are set to [DModeStereo, DModeSurround,
-- DMode16Bits, DModeSoftMusic, DModeSoftSndFX].
mikmodModifyDriverModeFlags :: ([DriverModeFlag] -> [DriverModeFlag]) -> IO ()
mikmodModifyDriverModeFlags f = do
  flags <- mikmodGetDriverModeFlags
  mikmodSetDriverModeFlags (f flags)

mikmodGetDriverModeFlags :: IO [DriverModeFlag]
mikmodGetDriverModeFlags = unpackFlags <$> peek c_md_mode

-- | See 'mikmodModifyDriverModeFlags' to avoid clobbering flags you aren't
-- trying to clear.
mikmodSetDriverModeFlags :: [DriverModeFlag] -> IO ()
mikmodSetDriverModeFlags flags = poke c_md_mode (packFlags flags)




-- | Initialize the MikMod system using an initialization string. An empty
-- string is acceptable (see MikMod docs for more info). If initialization
-- fails it will throw a MikModError.
--
-- Don't try this until you register a driver which is supported by your
-- system. See 'mikmodRegisterAllDrivers'.
--
-- See also the convenience functions 'mikmodSetup' and 'runMikMod'.
mikmodInit :: String -> IO ()
mikmodInit params = do
  r <- mikmodInitSafe params
  case r of
    Left e  -> throwIO (MikModException e)
    Right _ -> return ()

-- | Same as 'mikmodInit' but doesn't throw exceptions.
mikmodInitSafe :: String -> IO (Either MikModError ())
mikmodInitSafe params = withCString params $ \ptr -> do
  n <- c_MikMod_Init ptr
  if n == 0
    then return (Right ())
    else Left <$> mikmodGetError

-- | Registers all drivers and loaders, initializes MikMod, sets a number
-- of sample voices and enables output.
mikmodSetup :: Int -> IO ()
mikmodSetup sfxVoices = do
  mikmodRegisterAllDrivers
  mikmodRegisterAllLoaders
  mikmodInit ""
  mikmodSetNumVoices (-1) sfxVoices
  mikmodEnableOutput
  
-- | Run an action between 'mikmodSetup' and 'mikmodExit'. It does not handle
-- freeing of Modules or Samples.
runMikMod :: Int -> IO a -> IO a
runMikMod sfxVoices action = do
  mikmodSetup sfxVoices
  action `finally` mikmodExit

-- | Shutdown the MikMod system.
mikmodExit :: IO ()
mikmodExit = c_MikMod_Exit

-- | Returns True if and only if sound output is enabled.
mikmodActive :: IO Bool
mikmodActive = decodeBool <$> c_MikMod_Active

-- | Enable output. Playing modules will enable output automatically.
-- However playing samples does not. Therefore use 'mikmodEnableOutput' if
-- you intend to play sound effects with no music. The convenience function
-- 'mikmodSetup' enables output among other things.
mikmodEnableOutput :: IO ()
mikmodEnableOutput = c_MikMod_EnableOutput

-- | Disable output.
mikmodDisableOutput :: IO ()
mikmodDisableOutput = c_MikMod_DisableOutput

-- | Get the MikMod version as (major, minor, revision).
mikmodGetVersion :: IO (Int, Int, Int)
mikmodGetVersion = do
  enc <- fromIntegral <$> c_MikMod_GetVersion
  return (enc `shiftR` 16, 0xff .&. (enc `shiftR` 8), 0xff .&. enc)

-- | Get a formatted string describing the available drivers, if any.
mikmodInfoDriver :: IO (Maybe String)
mikmodInfoDriver = mikmodGetString c_MikMod_InfoDriver

-- | Get a formatted string describing the available loaders, if any.
mikmodInfoLoader :: IO (Maybe String)
mikmodInfoLoader = mikmodGetString c_MikMod_InfoLoader

-- | If your libmikmod has pthread support, returns True. Otherwise this
-- may initialize internal mutexes to support multi-threaded access anyway.
-- A result of True indicates this was successful. False indicates no support
-- for multi-threaded access is available. It is safe to call this multiple
-- times. Only the first call has any effect.
--
-- Short story: Before attempting to use MikMod from multiple threads execute
-- this and check that the result is True.
--
-- This only has an effect on Win32, OS/2, and EMX.
mikmodInitThreads :: IO Bool
mikmodInitThreads = decodeBool <$> c_MikMod_InitThreads

-- | Execute the action after calling MikMod_Lock. Calls MikMod_Unlock afterwards
-- even if an error occurred. If 'mikmodInitThreads' returns True then it means
-- all calls to libmikmod will be protected by internal mutexes. Therefore using
-- MikMod functions inside a 'withMikMod' will deadlock. Allowing clients to
-- manually lock MikMod is probably only useful when manipulating shared data
-- across the API boundary.
withMikMod :: IO a -> IO a
withMikMod = c_MikMod_Lock `bracket_` c_MikMod_Unlock

-- | Register all drivers. Use this before initializing MikMod with 'mikmodInit'.
mikmodRegisterAllDrivers :: IO ()
mikmodRegisterAllDrivers = c_MikMod_RegisterAllDrivers

-- | Register all loaders. Use this before loading any modules.
mikmodRegisterAllLoaders :: IO ()
mikmodRegisterAllLoaders = c_MikMod_RegisterAllLoaders

-- | Reset the driver using the new global variable settings.
-- If the driver has not been initialized, it will be now. Throws a
-- MikModError in case of failure.
mikmodReset :: String -> IO ()
mikmodReset params = do
  r <- mikmodResetSafe params
  case r of
    Left e  -> throwIO (MikModException e)
    Right _ -> return ()

-- | Same as 'mikmodReset' but doesn't throw exceptions.
mikmodResetSafe :: String -> IO (Either MikModError ())
mikmodResetSafe params = withCString params $ \ptr -> do
  n <- c_MikMod_Reset ptr
  if n == 0
    then return (Right ())
    else Left <$> mikmodGetError

-- | Set the number of music voices and sample voices to be used for playback.
-- If either parameter is -1, the currently set value will be retained.
--
-- In particular the number of music voices is handled by the module player
-- so probably shouldn't be manipulated. If this operation fails it will throw
-- a MikModError.
mikmodSetNumVoices :: Int -- ^ Number of music voices or -1
                   -> Int -- ^ Number of sample voices or -1
                   -> IO ()
mikmodSetNumVoices music sample = do
  r <- mikmodSetNumVoicesSafe music sample
  case r of
    Left e  -> throwIO (MikModException e)
    Right _ -> return ()

-- | Same as 'mikmodSetNumVoices' but doesn't throw exceptions.
mikmodSetNumVoicesSafe :: Int -> Int -> IO (Either MikModError ())
mikmodSetNumVoicesSafe music sample = do
  n <- c_MikMod_SetNumVoices (fromIntegral music) (fromIntegral sample)
  if n == 0
    then return (Right ())
    else Left <$> mikmodGetError

-- | Update the sound. If you don't call this often enough, then sound might drop
-- out. If you call this too often, the audio driver may eat CPU in a busy loop.
-- Higher quality audio requires calling mikmodUpdate more often (see
-- 'mikmodSetMixFreq'). And finally, on some drivers this is a no-op because
-- there is a fill thread.
--
-- Known:
--
-- - On OSX there is a fill thread and polling mikmodUpdate is unnecessary.
--
-- - On Linux ALSA there is /no/ fill thread and polling mikmodUpdate is necessary.
mikmodUpdate :: IO ()
mikmodUpdate = c_MikMod_Update

-- | Returns True if and only if a song is playing.
playerActive :: IO Bool
playerActive = decodeBool <$> c_Player_Active

-- | Free a module and all its contents. If the module was playing then it
-- will be stopped. Discard the ModuleHandle after using this operation.
playerFree :: ModuleHandle -> IO ()
playerFree = c_Player_Free

-- | Returns the voice corresponding to a module channel.
playerGetChannelVoice :: Int -> IO (Maybe Voice)
playerGetChannelVoice ch = do
  v <- c_Player_GetChannelVoice (fromIntegral ch)
  if v >= 0
    then return $ Just (Voice v)
    else return Nothing

-- | Get the currently playing module, if any.
playerGetModule :: IO (Maybe ModuleHandle)
playerGetModule = do
  ptr <- c_Player_GetModule
  if ptr == nullPtr
    then pure Nothing
    else pure (Just ptr)

-- | Load a module from a file. The second argument is the maximum number of
-- channels to allow. If something goes wrong while loading the module it will
-- throw a MikModError.
playerLoad :: FilePath -> Int -> CuriousFlag -> IO ModuleHandle
playerLoad path maxChans curious = do
  r <- playerLoadSafe path maxChans curious
  case r of
    Left e    -> throwIO (MikModException e)
    Right mod -> return mod

-- | Same as 'playerLoad' but doesn't throw exceptions.
playerLoadSafe :: FilePath -> Int -> CuriousFlag -> IO (Either MikModError ModuleHandle)
playerLoadSafe path maxChans curious = withCString path $ \cstr -> do
  ptr <- c_Player_Load cstr (fromIntegral maxChans) (marshalCurious curious)
  if ptr == nullPtr
    then Left <$> mikmodGetError
    else Right <$> pure ptr

-- | Same as 'playerLoad' but loads the module data from an MReader.
playerLoadGeneric :: MReader -> Int -> CuriousFlag -> IO ModuleHandle
playerLoadGeneric rd maxChans curious = do
  r <- playerLoadGenericSafe rd maxChans curious
  case r of
    Left e    -> throwIO (MikModException e)
    Right mod -> return mod

-- | Same as 'playerLoadGeneric' but doesn't throw exceptions.
playerLoadGenericSafe :: MReader -> Int -> CuriousFlag -> IO (Either MikModError ModuleHandle)
playerLoadGenericSafe rd maxChans curious = withMReader rd $ \rptr -> do
  mptr <- c_Player_LoadGeneric rptr (fromIntegral maxChans) (marshalCurious curious)
  if mptr == nullPtr
    then Left <$> mikmodGetError
    else Right <$> pure mptr

-- | Load only the title from a module file. Returns Nothing if there is
-- no title. If something goes wrong it will throw a MikModError.
playerLoadTitle :: FilePath -> IO (Maybe String)
playerLoadTitle path = do
  result <- playerLoadTitleSafe path
  case result of
    Left e -> throwIO (MikModException e)
    Right mtitle -> return mtitle


-- | Same as 'playerLoadTitle' but doesn't throw exceptions.
playerLoadTitleSafe :: FilePath -> IO (Either MikModError (Maybe String))
playerLoadTitleSafe path = do
  result <- withCString path c_Player_LoadTitle
  if result == nullPtr
    then do
      mme <- mikmodGetError
      if isNotAnError mme
        then return (Right Nothing)
        else return (Left mme)
    else Right . Just <$> peekCString result


-- | Mute a channel.
playerMuteChannel :: Int -> IO ()
playerMuteChannel ch = c_Player_MuteChannel (fromIntegral ch)

-- | Mute a range of channels. MuteOperation determines if the range is
-- inclusive or exclusive.
playerMuteChannels :: MuteOperation -> Int -> Int -> IO ()
playerMuteChannels op chanL chanU = c_Player_MuteChannels
  (marshalMuteOperation op)
  (fromIntegral chanL)
  (fromIntegral chanU)

-- | Return True if and only if a channel is muted.
playerMuted :: Int -> IO Bool
playerMuted ch = decodeBool <$> c_Player_Muted (fromIntegral ch)

-- | Skip to the next position in the current module.
playerNextPosition :: IO ()
playerNextPosition = c_Player_NextPosition

-- | Go back to the previous position in the current module.
playerPrevPosition :: IO ()
playerPrevPosition = c_Player_PrevPosition

-- | Returns True if and only if the player is paused.
playerPaused :: IO Bool
playerPaused = decodeBool <$> c_Player_Paused

-- | Set the position of the current module. 
playerSetPosition :: Int -> IO ()
playerSetPosition pos = c_Player_SetPosition (fromIntegral pos)

-- | Set the speed of the current module to a value in the range 1 to 32.
playerSetSpeed :: Int -> IO ()
playerSetSpeed speed = c_Player_SetSpeed (fromIntegral speed)

-- | Set the tempo of the current module to a value in the range 32 to 255.
playerSetTempo :: Int -> IO ()
playerSetTempo tempo = c_Player_SetTempo (fromIntegral tempo)

-- | Set the volume of the current module to a value in the range 0 to 128.
playerSetVolume :: Int -> IO ()
playerSetVolume volume = c_Player_SetVolume (fromIntegral volume)

-- | Begin playing a module. If another module is already playing it will
-- be stopped.
playerStart :: ModuleHandle -> IO ()
playerStart = c_Player_Start

-- | Stop the player.
playerStop :: IO ()
playerStop = c_Player_Stop

-- | Toggle the muting of the specified channel.
playerToggleMuteChannel :: Int -> IO ()
playerToggleMuteChannel ch = c_Player_ToggleMuteChannel (fromIntegral ch)

-- | Toggle the muting of a range of channels. MuteOperation determines if
-- the range is inclusive or exclusive.
playerToggleMuteChannels :: MuteOperation -> Int -> Int -> IO ()
playerToggleMuteChannels op chanL chanU = c_Player_ToggleMuteChannels
  (marshalMuteOperation op)
  (fromIntegral chanL)
  (fromIntegral chanU)

-- | Pause the player if it isn't paused. Otherwise unpause it.
playerTogglePause :: IO ()
playerTogglePause = c_Player_TogglePause

-- | Unmute the given channel.
playerUnmuteChannel :: Int -> IO ()
playerUnmuteChannel ch = c_Player_UnmuteChannel (fromIntegral ch)

-- | Toggle the muting of a range of channels. MuteOperation determines if the
-- range is inclusive or exclusive.
playerUnmuteChannels :: MuteOperation -> Int -> Int -> IO ()
playerUnmuteChannels op chanL chanU = c_Player_UnmuteChannels
  (marshalMuteOperation op)
  (fromIntegral chanL)
  (fromIntegral chanU)

-- | Load a sample from a mono, uncompressed RIFF WAV file. If something
-- goes wrong while loading the sample it will throw a MikModError.
sampleLoad :: FilePath -> IO SampleHandle
sampleLoad path = do
  r <- sampleLoadSafe path
  case r of
    Left e     -> throwIO (MikModException e)
    Right samp -> return samp

-- | Same as 'sampleLoad' but doesn't throw exceptions.
sampleLoadSafe :: FilePath -> IO (Either MikModError SampleHandle)
sampleLoadSafe path = withCString path $ \cstr -> do
  ptr <- c_Sample_Load cstr
  if ptr == nullPtr
    then Left <$> mikmodGetError
    else Right <$> pure ptr

-- | Same as 'sampleLoad' but read sample data from an MReader.
sampleLoadGeneric :: MReader -> IO SampleHandle
sampleLoadGeneric mr = do
  r <- sampleLoadGenericSafe mr
  case r of
    Left e     -> throwIO (MikModException e)
    Right samp -> return samp

-- | Same as 'sampleLoadGeneric' but doesn't throw exceptions.
sampleLoadGenericSafe :: MReader -> IO (Either MikModError SampleHandle)
sampleLoadGenericSafe mr = withMReader mr $ \rptr -> do
  sptr <- c_Sample_LoadGeneric rptr
  if sptr == nullPtr
    then Left <$> mikmodGetError
    else Right <$> pure sptr


-- | Plays a sound effects sample. Picks a voice from the number of voices
-- allocated for use as sound effects. Returns the voice that the sound is
-- being played on. The oldest playing sample will be interrupted if necessary,
-- unless all playing samples are "critical", in which case the sound will not
-- play.
--
-- The second argument is the position, in samples, to start playing from.
samplePlay :: SampleHandle -> Int -> IO (Maybe Voice)
samplePlay samp start = do
  v <- c_Sample_Play samp (fromIntegral start) 0
  if v >= 0
    then (return . Just . Voice) v
    else return Nothing

-- | Same behavior as 'samplePlay' except that the voice the sound is played
-- on (if any) is given the "critical" status. Note that this will still not
-- result in any critical samples being interrupted.
samplePlayCritical :: SampleHandle -> Int -> IO (Maybe Voice)
samplePlayCritical samp start = do
  v <- c_Sample_Play samp (fromIntegral start) sfxCritical
  if v >= 0
    then (return . Just . Voice) v
    else return Nothing

-- | Free a sample. Do not sampleFree samples aquired via 'getModuleSamples'.
-- Those are freed with 'playerFree'. Discard the SampleHandle after using
-- this operation.
sampleFree :: SampleHandle -> IO ()
sampleFree = c_Sample_Free

-- | Set a voice's volume to a value in the range 0 to 256. There are 257
-- volume levels.
voiceSetVolume :: Voice -> Int -> IO ()
voiceSetVolume v vol = c_Voice_SetVolume (marshalVoice v) (fromIntegral vol)

voiceGetVolume :: Voice -> IO Int
voiceGetVolume v = fromIntegral <$> c_Voice_GetVolume (marshalVoice v)

-- | Set a voice's frequency in Hertz.
voiceSetFrequency :: Voice -> Int -> IO ()
voiceSetFrequency v freq = c_Voice_SetFrequency (marshalVoice v) (fromIntegral freq)

voiceGetFrequency :: Voice -> IO Int
voiceGetFrequency v = fromIntegral <$> c_Voice_GetFrequency (marshalVoice v)

-- | Set a voice's pan position. 0 is far left. 127 is center. 255 is far right.
voiceSetPanning :: Voice -> Int -> IO ()
voiceSetPanning v pan = c_Voice_SetPanning (marshalVoice v) (fromIntegral pan)

voiceGetPanning :: Voice -> IO Int
voiceGetPanning v = fromIntegral <$> c_Voice_GetPanning (marshalVoice v)

-- | Play a sample on the specified voice starting from the specified position
-- in samples. The playing sample will have the same "critical status" as the
-- previous sample played on this voice.
voicePlay :: Voice -> SampleHandle -> Int -> IO ()
voicePlay v samp start = c_Voice_Play (marshalVoice v) samp (fromIntegral start)

-- | Stop a voice from playing.
voiceStop :: Voice -> IO ()
voiceStop v = c_Voice_Stop (marshalVoice v)

-- | Returns True if and only if the specified voice is /not/ playing.
voiceStopped :: Voice -> IO Bool
voiceStopped v = decodeBool <$> c_Voice_Stopped (marshalVoice v)

-- | Query the position (in samples) of the sample playing on this voice.
-- If no sample is playing it will return zero. On some drivers this operation
-- does not work and will return -1.
voiceGetPosition :: Voice -> IO Int
voiceGetPosition v = fromIntegral <$> c_Voice_GetPosition (marshalVoice v)

-- | Compute the "actual playing volume" of the voice. The result will be in
-- the range 0 - 65535. On some drivers this operation does not work and will
-- return zero.
voiceRealVolume :: Voice -> IO Int
voiceRealVolume v = fromIntegral <$> c_Voice_RealVolume (marshalVoice v)

