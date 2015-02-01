module Sound.MikMod 
(
  -- * Types  
  ModuleHandle,
  ModuleInfo(..),
  SampleHandle,
  SampleInfo(..),
  Voice(..),
  MDriverHandle,
  MDriverInfo,
  MuteOperation(..),
  MikModError(..),
  MikModException(..),

  -- * Core library operations
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

  -- * Module player operations
  playerLoad,
  playerLoadSafe,
  playerLoadCurious,
  playerLoadCuriousSafe,
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

  -- * Sample operations
  sampleLoad,
  sampleLoadSafe,
  samplePlay,
  samplePlayCritical,
  sampleFree,
  peekSample,

  -- * Voice operations
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

)
where

import Foreign.Ptr
import Foreign.C.String
import Data.Functor
import Control.Applicative
import Control.Exception
import Data.Bits

import Sound.MikMod.Synonyms
import Sound.MikMod.Types
import Sound.MikMod.Errors
import Sound.MikMod.Internal

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

playerLoadOpt :: FilePath -> Int -> Bool -> IO (Either MikModError ModuleHandle)
playerLoadOpt path maxChans curious = withCString path $ \cstr -> do
  ptr <- c_Player_Load cstr (fromIntegral maxChans) (encodeBool curious)
  if (ptr == nullPtr)
    then Left <$> mikmodGetError
    else return (Right ptr)

-- | Load a module from a file. The second argument is the maximum number of channels
-- to allow. If something goes wrong while loading the module it will throw a MikModError.
playerLoad :: FilePath -> Int -> IO ModuleHandle
playerLoad path maxChans = do
  r <- playerLoadSafe path maxChans
  case r of
    Left e    -> throwIO (MikModException e)
    Right mod -> return mod

-- | Same as playerLoad but doesn't throw exceptions.
playerLoadSafe :: FilePath -> Int -> IO (Either MikModError ModuleHandle)
playerLoadSafe path maxChans = playerLoadOpt path maxChans False

-- | Like 'playerLoad' but will try to play "hidden" tracks after the end of
-- the song. If something goes wrong while loading the module it will
-- throw a MikModError.
playerLoadCurious :: FilePath -> Int -> IO ModuleHandle
playerLoadCurious path maxChans = do
  r <- playerLoadCuriousSafe path maxChans
  case r of
    Left e    -> throwIO (MikModException e)
    Right mod -> return mod

-- | Same as 'playerLoadCurious' but doesn't throw exceptions.
playerLoadCuriousSafe :: FilePath -> Int -> IO (Either MikModError ModuleHandle)
playerLoadCuriousSafe path maxChans = playerLoadOpt path maxChans True

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


decodeBool :: BOOL -> Bool
decodeBool 0 = False
decodeBool 1 = True
decodeBool x = error ("decodeBool " ++ show x)

encodeBool :: Num a => Bool -> a
encodeBool False = 0
encodeBool True  = 1

mikmodGetString :: IO CString -> IO (Maybe String)
mikmodGetString query = do
  ptr <- query
  if (ptr == nullPtr)
    then return Nothing
    else
      Just <$> peekCString ptr
      `finally`
      c_MikMod_free ptr

