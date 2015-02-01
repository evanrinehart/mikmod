-- |
-- Module : Sound.MikMod
-- License : LGPL3
-- 
-- MikMod bindings for Haskell

module Sound.MikMod 
(
  -- * Overview
  -- $overview

  -- * Globals
  mikmodGetMusicVolume,
  mikmodSetMusicVolume,
  mikmodGetPanSep,
  mikmodSetPanSep,
  mikmodGetReverb,
  mikmodSetReverb,
  mikmodGetSndFXVolume,
  mikmodSetSndFXVolume,
  mikmodGetVolume,
  mikmodSetVolume,
  mikmodGetDeviceIndex,
  mikmodSetDeviceIndex,
  mikmodGetDriver,
  mikmodGetMixFreq,
  mikmodSetMixFreq,
  mikmodGetDriverModeFlags,
  mikmodSetDriverModeFlags,

  -- * Core Operations
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
  mikmodInitThreads,
  withMikMod,
  peekMDriver,

  -- * Module Player Operations
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
  peekModule,

  -- * Sample Operations
  sampleLoad,
  sampleLoadSafe,
  sampleLoadGeneric,
  sampleLoadGenericSafe,
  samplePlay,
  samplePlayCritical,
  sampleFree,
  peekSample,

  -- * Voice Operations
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
  newByteStringReader,

  -- * Types  
  Module,
  ModuleHandle,
  ModuleInfo(..),
  Sample,
  SampleHandle,
  SampleInfo(..),
  Voice(..),
  MuteOperation(..),
  CuriousFlag(..),
  MDriver,
  MDriverHandle,
  MDriverInfo,
  MikModError(..),
  MikModException(..),
  

)
where

import Foreign.Ptr
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

-- $overview
--
-- <http://mikmod.sourceforge.net/ MikMod> is a C library for 
-- mixing and playing music modules and sound samples. These bindings are
-- a low level interface to the library so to get a full understanding of how
-- to use it please refer to the MikMod documentation.
-- 
-- The user controls MikMod by manipulating a handful of global variables,
-- calling API functions, and manipulating fields of the Module and Sample
-- structure. These bindings only provide convenience wrappers to exactly
-- these things. Storable instances are not provided to avoid clobbering
-- fields which are not intended to be written by the client and to encourage
-- updating particular fields in large structures without a full replacement.
-- Modifying values of global variables and structure fields is allowed during
-- playback and will have immediate effect.
--
-- MikMod allows loading modules and samples from the file system using a
-- file path, but more customized loading can be accomplished by using a
-- MReader structure and the "generic" loading methods.


-- | Query the global music volume. It has range 0 to 128 (there are 129 possible
-- volume levels). The default music volume is 128.
mikmodGetMusicVolume :: IO Int
mikmodGetMusicVolume = fromIntegral <$> peek c_md_musicvolume

-- | Set the global music volume. The argument must be in the range 0 to 128.
mikmodSetMusicVolume :: Int -> IO ()
mikmodSetMusicVolume v = poke c_md_musicvolume (fromIntegral v)

-- | Query the global stereo separation. It has range 0 to 128 where 0 means
-- mono sound and 128 means full separation. The default pan sep is 128.
mikmodGetPanSep :: IO Int
mikmodGetPanSep = fromIntegral <$> peek c_md_pansep

-- | Set the global stereo separation. The argument must be in the range 0 to 128.
mikmodSetPanSep :: Int -> IO ()
mikmodSetPanSep v = poke c_md_pansep (fromIntegral v)

-- | Query the global reverb. It has range 0 to 15 where 0 means no reverb
-- and 15 means extreme reverb. The default reverb is zero.
mikmodGetReverb :: IO Int
mikmodGetReverb = fromIntegral <$> peek c_md_reverb

-- | Set the global reverb. The argument must be in the range 0 to 15.
mikmodSetReverb :: Int -> IO ()
mikmodSetReverb v = poke c_md_reverb (fromIntegral v)

-- | Query the global sound effects volume. It has range 0 to 128.
-- The default sound effects volume is 128.
mikmodGetSndFXVolume :: IO Int
mikmodGetSndFXVolume = fromIntegral <$> peek c_md_sndfxvolume

-- | Set the global sound effects volume. The argument must be in the range 0 to 128.
mikmodSetSndFXVolume :: Int -> IO ()
mikmodSetSndFXVolume v = poke c_md_sndfxvolume (fromIntegral v)

-- | Query the global overall sound volume. It has range 0 to 128. The default
-- overall sound volume is 128.
mikmodGetVolume :: IO Int
mikmodGetVolume = fromIntegral <$> peek c_md_volume

-- | Set the global overall sound volume. The argument must be in the range 0 to 128.
mikmodSetVolume :: Int -> IO ()
mikmodSetVolume v = poke c_md_volume (fromIntegral v)

-- | The selected output driver from the global 1-based list of drivers.
mikmodGetDeviceIndex :: IO Int
mikmodGetDeviceIndex = fromIntegral <$> peek c_md_device

-- | Change the selected output driver by specifying a 1-based into into the
-- global list of drivers. Setting this to zero, the default, means autodetect.
-- To see the list use 'mikmodInfoDriver'.
mikmodSetDeviceIndex :: Int -> IO ()
mikmodSetDeviceIndex i = poke c_md_device (fromIntegral i)

-- | Get a info report of the sound driver currently in use, if any. MikMod
-- does not expose any functionality via MDriver field manipulation.
mikmodGetDriver :: IO (Maybe MDriverInfo)
mikmodGetDriver = do
  r <- peek c_md_driver
  if (r == nullPtr)
    then return Nothing
    else Just <$> peekMDriver r

-- | Query the mix frequency setting measured in Hertz. Higher values mean
-- more sound quality and more CPU usage. The default is 44100.
mikmodGetMixFreq :: IO Int
mikmodGetMixFreq = fromIntegral <$> peek c_md_mixfreq

-- | Set the mix frequency measured in Hertz. Higher values mean more sound
-- quality and more CPU usage. Common values are 8000, 11025, 22100, and
-- 44100. The default is 44100.
mikmodSetMixFreq :: Int -> IO ()
mikmodSetMixFreq freq = poke c_md_mixfreq (fromIntegral freq)

-- | Query the current "mode flags". See 'mikmodSetDriverModeFlags'.
mikmodGetDriverModeFlags :: IO [DriverModeFlag]
mikmodGetDriverModeFlags = unpackFlags <$> peek c_md_mode

-- | Set the "mode flags". These flags affect sound output in various ways.
-- For a full explanation of each one see the MikMod docs. Changing DModeInterp,
-- DModeReverse, or DModeSurround will affect playing immediately. The other
-- flags will require a reset. The default flags are set to [DModeStereo, DModeSurround,
-- DMode16Bits, DModeSoftMusic, DModeSoftSndFX].
mikmodSetDriverModeFlags :: [DriverModeFlag] -> IO ()
mikmodSetDriverModeFlags flags = poke c_md_mode (packFlags flags)


-- | Initialize the MikMod system using an initialization string. An empty
-- string is acceptable (see MikMod docs for more info). If initialization
-- fails it will throw a MikModError.
--
-- Don't try this until you register a driver which is supported by your
-- system. See 'mikmodRegisterAllDrivers'.
mikmodInit :: String -> IO ()
mikmodInit params = do
  r <- mikmodInitSafe params
  case r of
    Left e  -> throwIO (MikModException e)
    Right _ -> return ()

-- | Same as mikmodInit but doesn't throw exceptions.
mikmodInitSafe :: String -> IO (Either MikModError ())
mikmodInitSafe params = withCString params $ \ptr -> do
  n <- c_MikMod_Init ptr
  if (n == 0)
    then return (Right ())
    else Left <$> mikmodGetError

-- | Shutdown the MikMod system.
mikmodExit :: IO ()
mikmodExit = c_MikMod_Exit

-- | Check if a module is currently playing.
mikmodActive :: IO Bool
mikmodActive = decodeBool <$> c_MikMod_Active

-- | Disable output.
mikmodDisableOutput :: IO ()
mikmodDisableOutput = c_MikMod_DisableOutput

-- | Reverses the effect of 'mikmodDisableOutput'. This will probably crash
-- if used before MikMod is initialized.
mikmodEnableOutput :: IO ()
mikmodEnableOutput = c_MikMod_EnableOutput

-- | Get the MikMod version as (major, minor, revision).
mikmodGetVersion :: IO (Int, Int, Int)
mikmodGetVersion = do
  enc <- fromIntegral <$> c_MikMod_GetVersion
  return (enc `shiftR` 16, 0xff .&. (enc `shiftR` 8), 0xff .&. enc)

-- | Get a formatted string describing the current driver, if any.
mikmodInfoDriver :: IO (Maybe String)
mikmodInfoDriver = mikmodGetString c_MikMod_InfoDriver

-- | Get a formatted string describing the current loaders, if any.
mikmodInfoLoader :: IO (Maybe String)
mikmodInfoLoader = mikmodGetString c_MikMod_InfoLoader

-- | Check if MikMod is thread safe.
mikmodInitThreads :: IO Bool
mikmodInitThreads = decodeBool <$> c_MikMod_InitThreads

-- | Execute the action after calling MikMod_Lock. Calls MikMod_Unlock afterwards
-- even if an error occurred. See the MikMod docs to determine if this is necessary.
withMikMod :: IO a -> IO a
withMikMod = c_MikMod_Lock `bracket_` c_MikMod_Unlock

-- | Register all drivers. Use this before initializing MikMod with 'mikmodInit'.
mikmodRegisterAllDrivers :: IO ()
mikmodRegisterAllDrivers = c_MikMod_RegisterAllDrivers

-- | Register all loaders. Use this before loading any modules or samples.
mikmodRegisterAllLoaders :: IO ()
mikmodRegisterAllLoaders = c_MikMod_RegisterAllLoaders

-- | Reinitialize the MikMod system. This might be necessary after tweaking
-- one of MikMods global parameters. If reinitialization fails it will throw
-- a MikModError.
mikmodReset :: String -> IO ()
mikmodReset params = do
  r <- mikmodResetSafe params
  case r of
    Left e  -> throwIO (MikModException e)
    Right _ -> return ()

-- | Same as mikmodReset but doesn't throw exceptions.
mikmodResetSafe :: String -> IO (Either MikModError ())
mikmodResetSafe params = withCString params $ \ptr -> do
  n <- c_MikMod_Reset ptr
  if (n == 0)
    then return (Right ())
    else Left <$> mikmodGetError

-- | Set the number of music voices and sample voices to be used for playback.
-- If this operation fails for some reason it will throw a MikModError.
mikmodSetNumVoices :: Int -> Int -> IO ()
mikmodSetNumVoices music sample = do
  r <- mikmodSetNumVoicesSafe music sample
  case r of
    Left e  -> throwIO (MikModException e)
    Right _ -> return ()

-- | Same as mikmodSetNumVoices but doesn't throw exceptions.
mikmodSetNumVoicesSafe :: Int -> Int -> IO (Either MikModError ())
mikmodSetNumVoicesSafe music sample = do
  n <- c_MikMod_SetNumVoices (fromIntegral music) (fromIntegral sample)
  if (n == 0)
    then return (Right ())
    else Left <$> mikmodGetError

-- | Bump MikMod to make it fill its audio output buffer. Must be called
-- periodically for playback to work. To prevent audio dropouts, it must be
-- called more often when the audio quality is higher than when it is lower.
-- You only need to call this while sound is playing.
mikmodUpdate :: IO ()
mikmodUpdate = c_MikMod_Update

-- | Check if the player is active.
playerActive :: IO Bool
playerActive = decodeBool <$> c_Player_Active

-- | Unload a module and stop it if it is playing. Discard the handle after
-- this operation.
playerFree :: ModuleHandle -> IO ()
playerFree ptr = c_Player_Free ptr

-- | This function determines the voice corresponding to the specified module channel.
playerGetChannelVoice :: Int -> IO Voice
playerGetChannelVoice ch = Voice <$> c_Player_GetChannelVoice (fromIntegral ch)

-- | Get the currently playing module, if any.
playerGetModule :: IO (Maybe ModuleHandle)
playerGetModule = do
  ptr <- c_Player_GetModule
  if (ptr == nullPtr)
    then return Nothing
    else return (Just ptr)

-- | Load a module from a file. The second argument is the maximum number of channels
-- to allow. If something goes wrong while loading the module it will throw a MikModError.
playerLoad :: FilePath -> Int -> CuriousFlag -> IO ModuleHandle
playerLoad path maxChans curious = do
  r <- playerLoadSafe path maxChans curious
  case r of
    Left e    -> throwIO (MikModException e)
    Right mod -> return mod

-- | Same as playerLoad but doesn't throw exceptions.
playerLoadSafe :: FilePath -> Int -> CuriousFlag -> IO (Either MikModError ModuleHandle)
playerLoadSafe path maxChans curious = withCString path $ \cstr -> do
  ptr <- c_Player_Load cstr (fromIntegral maxChans) (marshalCurious curious)
  if (ptr == nullPtr)
    then Left <$> mikmodGetError
    else return (Right ptr)

-- | Same as playerLoad but loads the module data from the MReader.
playerLoadGeneric :: MReader -> Int -> CuriousFlag -> IO ModuleHandle
playerLoadGeneric rd maxChans curious = do
  r <- playerLoadGenericSafe rd maxChans curious
  case r of
    Left e    -> throwIO (MikModException e)
    Right mod -> return mod

-- | Same as playerLoadGeneric but doesn't throw exceptions.
playerLoadGenericSafe :: MReader -> Int -> CuriousFlag -> IO (Either MikModError ModuleHandle)
playerLoadGenericSafe rd maxChans curious = withMReader rd $ \rptr -> do
  mptr <- c_Player_LoadGeneric rptr (fromIntegral maxChans) (marshalCurious curious)
  if (mptr == nullPtr)
    then Left <$> mikmodGetError
    else return (Right mptr)

-- | Load only the title from a module file. Returns Nothing in case there
-- is no title or an error occurred!
playerLoadTitle :: FilePath -> IO (Maybe String)
playerLoadTitle path = mikmodGetString (withCString path c_Player_LoadTitle)

-- | Mute the given channel.
playerMuteChannel :: Int -> IO ()
playerMuteChannel ch = c_Player_MuteChannel (fromIntegral ch)

-- | Mute a range of channels. If MuteInclusive is used it will include
-- all channels within the range. MuteExclusive is the opposite of MuteInclusive.
playerMuteChannels :: MuteOperation -> Int -> Int -> IO ()
playerMuteChannels op chanL chanU = c_Player_MuteChannels
  (marshalMuteOperation op)
  (fromIntegral chanL)
  (fromIntegral chanU)

-- | Check if a channel is muted.
playerMuted :: Int -> IO Bool
playerMuted ch = decodeBool <$> c_Player_Muted (fromIntegral ch)

-- | Skip to the next position in the module.
playerNextPosition :: IO ()
playerNextPosition = c_Player_NextPosition

-- | Go back to the previous position in the module.
playerPrevPosition :: IO ()
playerPrevPosition = c_Player_PrevPosition

-- | Check if the current module is paused.
playerPaused :: IO Bool
playerPaused = decodeBool <$> c_Player_Paused

-- | Set the position of the current module. 
playerSetPosition :: Int -> IO ()
playerSetPosition pos = c_Player_SetPosition (fromIntegral pos)

-- | Set the speed of the current module, 1 to 32.
playerSetSpeed :: Int -> IO ()
playerSetSpeed speed = c_Player_SetSpeed (fromIntegral speed)

-- | Set the tempo of the current module, 32 to 255.
playerSetTempo :: Int -> IO ()
playerSetTempo tempo = c_Player_SetTempo (fromIntegral tempo)

-- | Set the volume of the current module, 0 to 128.
playerSetVolume :: Int -> IO ()
playerSetVolume volume = c_Player_SetVolume (fromIntegral volume)

-- | Begin playing the given module.
playerStart :: ModuleHandle -> IO ()
playerStart = c_Player_Start

-- | Stop the player.
playerStop :: IO ()
playerStop = c_Player_Stop

-- | Toggle the muting of the specified channel.
playerToggleMuteChannel :: Int -> IO ()
playerToggleMuteChannel ch = c_Player_ToggleMuteChannel (fromIntegral ch)

-- | Toggle the muting of a range of channels. If MuteInclusive is used this will
-- include all channels in the given range. MuteExclusive is the opposite of
-- MuteInclusive.
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

-- | Unmute a range of channels. If MuteInclusive is used this will include
-- all channels in the given range. MuteExclusive is the opposite of MuteInclusive.
playerUnmuteChannels :: MuteOperation -> Int -> Int -> IO ()
playerUnmuteChannels op chanL chanU = c_Player_UnmuteChannels
  (marshalMuteOperation op)
  (fromIntegral chanL)
  (fromIntegral chanU)

-- | Unload a sample. Discard the handle after this operation.
sampleFree :: SampleHandle -> IO ()
sampleFree = c_Sample_Free

-- | Load a sample from a mono, uncompressed RIFF WAV file. If something
-- goes wrong while loading the same it will throw a MikModError.
sampleLoad :: FilePath -> IO SampleHandle
sampleLoad path = do
  r <- sampleLoadSafe path
  case r of
    Left e     -> throwIO (MikModException e)
    Right samp -> return samp

-- | Same as sampleLoad but doesn't throw exceptions.
sampleLoadSafe :: FilePath -> IO (Either MikModError SampleHandle)
sampleLoadSafe path = withCString path $ \cstr -> do
  ptr <- c_Sample_Load cstr
  if (ptr == nullPtr)
    then Left <$> mikmodGetError
    else return (Right ptr)

-- | Same as sampleLoad but read sample data from a MReader.
sampleLoadGeneric :: MReader -> IO SampleHandle
sampleLoadGeneric mr = do
  r <- sampleLoadGenericSafe mr
  case r of
    Left e     -> throwIO (MikModException e)
    Right samp -> return samp

-- | Same as sampleLoadGeneric but doesn't throw exceptions.
sampleLoadGenericSafe :: MReader -> IO (Either MikModError SampleHandle)
sampleLoadGenericSafe mr = withMReader mr $ \rptr -> do
  sptr <- c_Sample_LoadGeneric rptr
  if (sptr == nullPtr)
    then Left <$> mikmodGetError
    else return (Right sptr)


-- | Play the given sample from the specified starting position (in samples).
-- If there aren't enough voices available to do this, it will replace the
-- oldest non-critical sample currently playing.
samplePlay :: SampleHandle -> Int -> IO Voice
samplePlay ptr start = Voice <$> c_Sample_Play ptr (fromIntegral start) 0

-- | This is like 'samplePlay' but the sample will not be interrupted by other
-- samples played later (unless all voices are being used by critical samples
-- and yet another critical sample is played).
samplePlayCritical :: SampleHandle -> Int -> IO Voice
samplePlayCritical ptr start = Voice <$> c_Sample_Play ptr (fromIntegral start) sfxCritical

-- | Set a voice's volume, 0 - 256. There are 257 volume levels.
voiceSetVolume :: Voice -> Int -> IO ()
voiceSetVolume v vol = c_Voice_SetVolume (marshalVoice v) (fromIntegral vol)

-- | Query a voice's current volume level.
voiceGetVolume :: Voice -> IO Int
voiceGetVolume v = fromIntegral <$> c_Voice_GetVolume (marshalVoice v)

-- | Set a voice's frequency in Hertz.
voiceSetFrequency :: Voice -> Int -> IO ()
voiceSetFrequency v freq = c_Voice_SetFrequency (marshalVoice v) (fromIntegral freq)

-- | Query a voice's current frequency in Hertz.
voiceGetFrequency :: Voice -> IO Int
voiceGetFrequency v = fromIntegral <$> c_Voice_GetFrequency (marshalVoice v)

-- | Set a voice's pan position. 0 is far left, 127 is center, 255 is far right.
voiceSetPanning :: Voice -> Int -> IO ()
voiceSetPanning v pan = c_Voice_SetPanning (marshalVoice v) (fromIntegral pan)

-- | Query a voice's current pan position. This will give 127 (center) when
-- no sample is playing on the given voice.
voiceGetPanning :: Voice -> IO Int
voiceGetPanning v = fromIntegral <$> c_Voice_GetPanning (marshalVoice v)

-- | Play a sample on the specified voice. The playing sample will have the
-- same "critical status" as the previous sample played on this voice.
voicePlay :: Voice -> SampleHandle -> Int -> IO ()
voicePlay v samp start = c_Voice_Play (marshalVoice v) samp (fromIntegral start)

-- | Stop a voice from playing.
voiceStop :: Voice -> IO ()
voiceStop v = c_Voice_Stop (marshalVoice v)

-- | Check if a voice is currently not playing.
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

mikmodGetString :: IO CString -> IO (Maybe String)
mikmodGetString query = do
  ptr <- query
  if (ptr == nullPtr)
    then return Nothing
    else
      Just <$> peekCString ptr
      `finally`
      c_MikMod_free ptr

