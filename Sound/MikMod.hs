module Sound.MikMod where

import Foreign.Ptr
import Foreign.C.String
import Data.Functor
import Control.Applicative
import Control.Exception
import Data.Bits

import Sound.MikMod.Types
import Sound.MikMod.Errors
import Sound.MikMod.Internal

-- | Initialize the MikMod system.
mikmodInit :: String -> IO (Either MikModError ())
mikmodInit params = withCString params $ \ptr -> do
  n <- c_MikMod_Init ptr
  if (n == 0)
    then return (Right ())
    else Left <$> mikmodGetError

-- | Shutdown the MikMod system
mikmodExit :: IO ()
mikmodExit = c_MikMod_Exit

-- | Check if a module is currently playing
mikmodActive :: IO Bool
mikmodActive = decodeBool <$> c_MikMod_Active

-- | Disable output
mikmodDisableOutput :: IO ()
mikmodDisableOutput = c_MikMod_DisableOutput

-- | Enable output
mikmodEnableOutput :: IO ()
mikmodEnableOutput = c_MikMod_EnableOutput

-- | Get the MikMod version (major, minor, revision)
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
-- even if an error occurred. 
withMikmod :: IO a -> IO a
withMikmod = c_MikMod_Lock `bracket_` c_MikMod_Unlock

-- | Register all drivers.
mikmodRegisterAllDrivers :: IO ()
mikmodRegisterAllDrivers = c_MikMod_RegisterAllDrivers

-- | Register all loaders.
mikmodRegisterAllLoaders :: IO ()
mikmodRegisterAllLoaders = c_MikMod_RegisterAllLoaders

-- | Reinitialize the MikMod system.
mikmodReset :: String -> IO (Either MikModError ())
mikmodReset params = withCString params $ \ptr -> do
  n <- c_MikMod_Reset ptr
  if (n == 0)
    then return (Right ())
    else Left <$> mikmodGetError

-- | Set the number of music voices and sample voices to be used for playback.
mikmodSetNumVoices :: Int -> Int -> IO (Either MikModError ())
mikmodSetNumVoices music sample = do
  n <- c_MikMod_SetNumVoices (fromIntegral music) (fromIntegral sample)
  if (n == 0)
    then return (Right ())
    else Left <$> mikmodGetError

-- | Poke MikMod to make it fill the output buffer. Must be called periodically
-- for playback to work.
mikmodUpdate :: IO ()
mikmodUpdate = c_MikMod_Update


-- | Check if the player is active.
playerActive :: IO Bool
playerActive = decodeBool <$> c_Player_Active

-- | Unload a module and stop it if it is playing. Don't use the module
-- handle after doing this.
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

-- | Load a module from a file allowed the given maximum number of channels.
playerLoad :: FilePath -> Int -> IO (Either MikModError ModuleHandle)
playerLoad path maxChans = playerLoadOpt path maxChans False

-- | Like playerLoad but will try to play "hidden" tracks after the end of
-- the song.
playerLoadCurious :: FilePath -> Int -> IO (Either MikModError ModuleHandle)
playerLoadCurious path maxChans = playerLoadOpt path maxChans True

-- | Load only the title from a module file. Returns Nothing in case there
-- is no title or an error occurred!
playerLoadTitle :: FilePath -> IO (Maybe String)
playerLoadTitle path = mikmodGetString (withCString path c_Player_LoadTitle)

playerMuteChannel :: Int -> IO ()
playerMuteChannel ch = c_Player_MuteChannel (fromIntegral ch)

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

playerStart :: ModuleHandle -> IO ()
playerStart = c_Player_Start

playerStop :: IO ()
playerStop = c_Player_Stop

playerToggleMuteChannel :: Int -> IO ()
playerToggleMuteChannel ch = c_Player_ToggleMuteChannel (fromIntegral ch)

playerToggleMuteChannels :: MuteOperation -> Int -> Int -> IO ()
playerToggleMuteChannels op chanL chanU = c_Player_ToggleMuteChannels
  (marshalMuteOperation op)
  (fromIntegral chanL)
  (fromIntegral chanU)

playerTogglePause :: IO ()
playerTogglePause = c_Player_TogglePause

playerUnmuteChannel :: Int -> IO ()
playerUnmuteChannel ch = c_Player_UnmuteChannel (fromIntegral ch)

playerUnmuteChannels :: MuteOperation -> Int -> Int -> IO ()
playerUnmuteChannels op chanL chanU = c_Player_UnmuteChannels
  (marshalMuteOperation op)
  (fromIntegral chanL)
  (fromIntegral chanU)

sampleFree :: SampleHandle -> IO ()
sampleFree = c_Sample_Free

sampleLoad :: FilePath -> IO (Either MikModError SampleHandle)
sampleLoad path = withCString path $ \cstr -> do
  ptr <- c_Sample_Load cstr
  if (ptr == nullPtr)
    then Left <$> mikmodGetError
    else return (Right ptr)

samplePlay :: SampleHandle -> Int -> IO Voice
samplePlay ptr start = Voice <$> c_Sample_Play ptr (fromIntegral start) 0

samplePlayCritical :: SampleHandle -> Int -> IO Voice
samplePlayCritical ptr start = Voice <$> c_Sample_Play ptr (fromIntegral start) sfxCritical

voiceSetVolume :: Voice -> Int -> IO ()
voiceSetVolume v vol = c_Voice_SetVolume (marshalVoice v) (fromIntegral vol)

voiceGetVolume :: Voice -> IO Int
voiceGetVolume v = fromIntegral <$> c_Voice_GetVolume (marshalVoice v)

voiceSetFrequency :: Voice -> Int -> IO ()
voiceSetFrequency v freq = c_Voice_SetFrequency (marshalVoice v) (fromIntegral freq)

{-
foreign import ccall unsafe "mikmod.h Voice_GetFrequency" c_Voice_GetFrequency :: SBYTE -> IO ULONG
foreign import ccall unsafe "mikmod.h Voice_SetPanning" c_Voice_SetPanning :: SBYTE -> ULONG -> IO ()
foreign import ccall unsafe "mikmod.h Voice_GetPanning" c_Voice_GetPanning :: SBYTE -> IO ULONG
foreign import ccall unsafe "mikmod.h Voice_Play" c_Voice_Play :: SBYTE -> Ptr Sample -> ULONG -> IO ()
foreign import ccall unsafe "mikmod.h Voice_Stop" c_Voice_Stop :: SBYTE -> IO ()
foreign import ccall unsafe "mikmod.h Voice_Stopped" c_Voice_Stopped :: SBYTE -> IO BOOL
foreign import ccall unsafe "mikmod.h Voice_GetPosition" c_Voice_GetPosition :: SBYTE -> IO SLONG
foreign import ccall unsafe "mikmod.h Voice_RealVolume" c_Voice_RealVolume :: SBYTE -> IO ULONG

-}


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

