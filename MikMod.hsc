{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Sound.MikMod where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Functor
import Control.Applicative
import Control.Exception
import Data.Bits
import System.IO.Unsafe

#include "mikmod.h"

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

-- globals
foreign import ccall "&MikMod_errno" c_MikMod_errno :: Ptr CInt
foreign import ccall "&MikMod_critical" c_MikMod_critical :: Ptr CInt

foreign import ccall "&md_musicvolume" c_md_musicvolume :: Ptr UBYTE
foreign import ccall "&md_pansep" c_md_pansep :: Ptr UBYTE
foreign import ccall "&md_reverb" c_md_reverb :: Ptr UBYTE
foreign import ccall "&md_sndfxvolume" c_md_sndfxvolume :: Ptr UBYTE
foreign import ccall "&md_volume" c_md_volume :: Ptr UBYTE

foreign import ccall "&md_device"  c_md_device  :: Ptr UWORD
foreign import ccall "&md_driver"  c_md_driver  :: Ptr (Ptr MDriver)
foreign import ccall "&md_mixfreq" c_md_mixfreq :: Ptr UWORD
foreign import ccall "&md_mode"    c_md_mode    :: Ptr UWORD

data MDriver
data MDriverInfo = MDriverInfo
  { mdriverName :: String
  , mdriverHardVoiceLimit :: Int
  , mdriverSoftVoiceLimit :: Int
  , mdriverAlias :: String
  } deriving (Show)
type MDriverHandle = Ptr MDriver

peekMDriver :: Ptr MDriver -> IO MDriverInfo
peekMDriver ptr = do
  name  <- peekCString $ (#ptr MDRIVER, Name) ptr
  alias <- peekCString $ (#ptr MDRIVER, Alias) ptr
  hard  <- fromIntegral <$> (peek $ (#ptr MDRIVER, HardVoiceLimit) ptr :: IO CUChar)
  soft  <- fromIntegral <$> (peek $ (#ptr MDRIVER, SoftVoiceLimit) ptr :: IO CUChar)
  return $ MDriverInfo name hard soft alias

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

peekModule :: Ptr Module -> IO ModuleInfo
peekModule ptr = ModuleInfo <$>
  (peekCString $ (#ptr MODULE, songname) ptr) <*>
  (peekCString $ (#ptr MODULE, modtype) ptr)

pokeModuleBPM :: Ptr Module -> Int -> IO ()
pokeModuleBPM ptr bpm = (#poke MODULE, bpm) ptr (fromIntegral bpm :: UWORD)

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

pokeSampleVolume :: SampleHandle -> Int -> IO ()
pokeSampleVolume ptr vol = (#poke SAMPLE, volume) ptr (fromIntegral vol :: UBYTE)

newtype SampleFlag = SampleFlag { marshalSampleFlag :: CUShort }
  deriving (Eq, Show)

newtype DriverModeFlag = DriverModeFlag CInt deriving (Eq, Ord, Show)
#{enum DriverModeFlag, DriverModeFlag,
    DMODE_16BITS,
    DMODE_STEREO,
    DMODE_SOFT_SNDFX,
    DMODE_SOFT_MUSIC,
    DMODE_HQMIXER,
    DMODE_FLOAT,
    DMODE_SURROUND,
    DMODE_INTERP,
    DMODE_REVERSE,
    DMODE_SIMDMIXER,
    DMODE_NOISEREDUCTION }

foreign import ccall unsafe "mikmod.h MikMod_Active" c_MikMod_Active :: IO BOOL
foreign import ccall unsafe "mikmod.h MikMod_DisableOutput" c_MikMod_DisableOutput :: IO ()
foreign import ccall unsafe "mikmod.h MikMod_EnableOutput" c_MikMod_EnableOutput :: IO ()
foreign import ccall unsafe "mikmod.h MikMod_Exit" c_MikMod_Exit :: IO ()
foreign import ccall unsafe "mikmod.h MikMod_GetVersion" c_MikMod_GetVersion :: IO CInt
foreign import ccall unsafe "mikmod.h MikMod_InfoDriver" c_MikMod_InfoDriver :: IO CString
foreign import ccall unsafe "mikmod.h MikMod_InfoLoader" c_MikMod_InfoLoader :: IO CString
foreign import ccall unsafe "mikmod.h MikMod_Init" c_MikMod_Init :: CString -> IO CInt
foreign import ccall unsafe "mikmod.h MikMod_InitThreads" c_MikMod_InitThreads :: IO CInt
foreign import ccall unsafe "mikmod.h MikMod_Lock" c_MikMod_Lock :: IO ()
foreign import ccall unsafe "mikmod.h MikMod_Unlock" c_MikMod_Unlock :: IO ()
foreign import ccall unsafe "mikmod.h MikMod_RegisterAllDrivers" c_MikMod_RegisterAllDrivers :: IO ()
foreign import ccall unsafe "mikmod.h MikMod_RegisterAllLoaders" c_MikMod_RegisterAllLoaders :: IO ()
foreign import ccall unsafe "mikmod.h MikMod_Reset" c_MikMod_Reset :: CString -> IO CInt
foreign import ccall unsafe "mikmod.h MikMod_SetNumVoices" c_MikMod_SetNumVoices :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "mikmod.h MikMod_Update" c_MikMod_Update :: IO ()
foreign import ccall unsafe "mikmod.h MikMod_strerror" c_MikMod_strerror :: CInt -> IO CString

foreign import ccall unsafe "mikmod.h Player_Active" c_Player_Active :: IO CInt
foreign import ccall unsafe "mikmod.h Player_Free" c_Player_Free :: Ptr Module -> IO ()
foreign import ccall unsafe "mikmod.h Player_GetChannelVoice" c_Player_GetChannelVoice :: UBYTE -> IO SBYTE
foreign import ccall unsafe "mikmod.h Player_GetModule" c_Player_GetModule :: IO (Ptr Module)
foreign import ccall unsafe "mikmod.h Player_Load" c_Player_Load :: CString -> CInt -> CInt -> IO (Ptr Module)
foreign import ccall unsafe "mikmod.h Player_LoadTitle" c_Player_LoadTitle :: CString -> IO CString
foreign import ccall unsafe "mikmod.h Player_Mute" c_Player_MuteChannel :: CInt -> IO ()
foreign import ccall unsafe "mikmod.h Player_Mute" c_Player_MuteChannels :: CInt -> CInt -> CInt -> IO ()
foreign import ccall unsafe "mikmod.h Player_Muted" c_Player_Muted :: UBYTE -> IO CInt
foreign import ccall unsafe "mikmod.h Player_NextPosition" c_Player_NextPosition :: IO ()
foreign import ccall unsafe "mikmod.h Player_PrevPosition" c_Player_PrevPosition :: IO ()
foreign import ccall unsafe "mikmod.h Player_Paused" c_Player_Paused :: IO CInt
foreign import ccall unsafe "mikmod.h Player_SetPosition" c_Player_SetPosition :: UWORD -> IO ()
foreign import ccall unsafe "mikmod.h Player_SetSpeed" c_Player_SetSpeed :: UWORD -> IO ()
foreign import ccall unsafe "mikmod.h Player_SetTempo" c_Player_SetTempo :: UWORD -> IO ()
foreign import ccall unsafe "mikmod.h Player_SetVolume" c_Player_SetVolume :: SWORD -> IO ()
foreign import ccall unsafe "mikmod.h Player_Start" c_Player_Start :: Ptr Module -> IO ()
foreign import ccall unsafe "mikmod.h Player_Stop" c_Player_Stop :: IO ()
foreign import ccall unsafe "mikmod.h Player_ToggleMute" c_Player_ToggleMuteChannel :: CInt -> IO ()
foreign import ccall unsafe "mikmod.h Player_ToggleMute" c_Player_ToggleMuteChannels :: CInt -> CInt -> CInt -> IO ()
foreign import ccall unsafe "mikmod.h Player_TogglePause" c_Player_TogglePause :: IO ()
foreign import ccall unsafe "mikmod.h Player_Unmute" c_Player_UnmuteChannel :: CInt -> IO ()
foreign import ccall unsafe "mikmod.h Player_Unmute" c_Player_UnmuteChannels :: CInt -> CInt -> CInt -> IO ()

foreign import ccall unsafe "mikmod.h Sample_Free" c_Sample_Free :: Ptr Sample -> IO ()
foreign import ccall unsafe "mikmod.h Sample_Load" c_Sample_Load :: CString -> IO (Ptr Sample)
foreign import ccall unsafe "mikmod.h Sample_Play" c_Sample_Play :: Ptr Sample -> ULONG -> UBYTE -> IO SBYTE

foreign import ccall unsafe "mikmod.h Voice_SetVolume" c_Voice_SetVolume :: SBYTE -> UWORD -> IO ()
foreign import ccall unsafe "mikmod.h Voice_GetVolume" c_Voice_GetVolume :: SBYTE -> IO UWORD
foreign import ccall unsafe "mikmod.h Voice_SetFrequency" c_Voice_SetFrequency :: SBYTE -> ULONG -> IO ()
foreign import ccall unsafe "mikmod.h Voice_GetFrequency" c_Voice_GetFrequency :: SBYTE -> IO ULONG
foreign import ccall unsafe "mikmod.h Voice_SetPanning" c_Voice_SetPanning :: SBYTE -> ULONG -> IO ()
foreign import ccall unsafe "mikmod.h Voice_GetPanning" c_Voice_GetPanning :: SBYTE -> IO ULONG
foreign import ccall unsafe "mikmod.h Voice_Play" c_Voice_Play :: SBYTE -> Ptr Sample -> ULONG -> IO ()
foreign import ccall unsafe "mikmod.h Voice_Stop" c_Voice_Stop :: SBYTE -> IO ()
foreign import ccall unsafe "mikmod.h Voice_Stopped" c_Voice_Stopped :: SBYTE -> IO BOOL
foreign import ccall unsafe "mikmod.h Voice_GetPosition" c_Voice_GetPosition :: SBYTE -> IO SLONG
foreign import ccall unsafe "mikmod.h Voice_RealVolume" c_Voice_RealVolume :: SBYTE -> IO ULONG

foreign import ccall unsafe "mikmod.h MikMod_free" c_MikMod_free :: Ptr a -> IO ()

marshalMuteOperation :: MuteOperation -> CInt
marshalMuteOperation MuteInclusive = (#const MUTE_INCLUSIVE)
marshalMuteOperation MuteExclusive = (#const MUTE_EXCLUSIVE)

mikmodGetError :: IO MikModError
mikmodGetError = fmap unmarshalMikModError (peek c_MikMod_errno)

showMikModError :: MikModError -> String
showMikModError e = unsafePerformIO $ do
  ptr <- c_MikMod_strerror (marshalMikModError e)
  peekCString ptr

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

-- nice haskell wrappers

mikmodActive :: IO Bool
mikmodActive = decodeBool <$> c_MikMod_Active

mikmodGetVersion :: IO (Int, Int, Int)
mikmodGetVersion = do
  enc <- fromIntegral <$> c_MikMod_GetVersion
  return (enc `shiftR` 16, 0xff .&. (enc `shiftR` 8), 0xff .&. enc)

mikmodInfoDriver :: IO (Maybe String)
mikmodInfoDriver = mikmodGetString c_MikMod_InfoDriver

mikmodInfoLoader :: IO (Maybe String)
mikmodInfoLoader = mikmodGetString c_MikMod_InfoLoader

-- | Initialize the MikMod system.
mikmodInit :: String -> IO (Either MikModError ())
mikmodInit params = withCString params $ \ptr -> do
  n <- c_MikMod_Init ptr
  if (n == 0)
    then return (Right ())
    else Left <$> mikmodGetError

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
samplePlayCritical ptr start = Voice <$> c_Sample_Play ptr (fromIntegral start) (#const SFX_CRITICAL)

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




data MikModError =

unmarshalMikModError :: CInt -> MikModError
unmarshalMikModError n = case n of
  (#const MMERR_OPENING_FILE) -> MMErrOpeningFile
  _ -> error ("unmarshalMikModError: unknown errno "++ show n)

marshalMikModError :: MikModError -> CInt
marshalMikModError e = case e of
  MMErrOpeningFile -> (#const MMERR_OPENING_FILE)
  MMERR_OUT_OF_MEMORY -> (#const MMERR_OUT_OF_MEMORY)
  MMERR_DYNAMIC_LINKING -> (#const MMERR_DYNAMIC_LINKING)

  MMERR_SAMPLE_TOO_BIG |
  MMERR_OUT_OF_HANDLES |
  MMERR_UNKNOWN_WAVE_TYPE |

  MMERR_LOADING_PATTERN |
  MMERR_LOADING_TRACK |
  MMERR_LOADING_HEADER |
  MMERR_LOADING_SAMPLEINFO,
  MMERR_NOT_A_MODULE,
  MMERR_NOT_A_STREAM,
  MMERR_MED_SYNTHSAMPLES,
  MMERR_ITPACK_INVALID_DATA,

  MMERR_DETECTING_DEVICE,
  MMERR_INVALID_DEVICE,
  MMERR_INITIALIZING_MIXER,
  MMERR_OPENING_AUDIO,
  MMERR_8BIT_ONLY |
  MMERR_16BIT_ONLY |
  MMERR_STEREO_ONLY |
  MMERR_ULAW |
  MMERR_NON_BLOCK |

  MMERR_AF_AUDIO_PORT |

  MMERR_AIX_CONFIG_INIT |
  MMERR_AIX_CONFIG_CONTROL |
  MMERR_AIX_CONFIG_START |

  MMERR_GUS_SETTINGS |
  MMERR_GUS_RESET |
  MMERR_GUS_TIMER |

  MMERR_HP_SETSAMPLESIZE |
  MMERR_HP_SETSPEED |
  MMERR_HP_CHANNELS |
  MMERR_HP_AUDIO_OUTPUT |
  MMERR_HP_AUDIO_DESC |
  MMERR_HP_BUFFERSIZE |

  MMERR_OSS_SETFRAGMENT |
  MMERR_OSS_SETSAMPLESIZE |
  MMERR_OSS_SETSTEREO |
  MMERR_OSS_SETSPEED |

  MMERR_SGI_SPEED |
  MMERR_SGI_16BIT |
  MMERR_SGI_8BIT |
  MMERR_SGI_STEREO |
  MMERR_SGI_MONO |

  MMERR_SUN_INIT |

  MMERR_OS2_MIXSETUP |
  MMERR_OS2_SEMAPHORE |
  MMERR_OS2_TIMER |
  MMERR_OS2_THREAD |

  MMERR_DS_PRIORITY |
  MMERR_DS_BUFFER |
  MMERR_DS_FORMAT |
  MMERR_DS_NOTIFY |
  MMERR_DS_EVENT |
  MMERR_DS_THREAD |
  MMERR_DS_UPDATE |

  MMERR_WINMM_HANDLE |
  MMERR_WINMM_ALLOCATED |
  MMERR_WINMM_DEVICEID |
  MMERR_WINMM_FORMAT |
  MMERR_WINMM_UNKNOWN |

  MMERR_MAC_SPEED |
  MMERR_MAC_START |

  MMERR_OSX_UNKNOWN_DEVICE |
  MMERR_OSX_BAD_PROPERTY |
  MMERR_OSX_UNSUPPORTED_FORMAT |
  MMERR_OSX_SET_STEREO |
  MMERR_OSX_BUFFER_ALLOC |
  MMERR_OSX_ADD_IO_PROC |
  MMERR_OSX_DEVICE_START |
  MMERR_OSX_PTHREAD |

  MMERR_DOSWSS_STARTDMA |
  MMERR_DOSSB_STARTDMA |

  MMERR_NO_FLOAT32 |

  MMERR_OPENAL_CREATECTX |
  MMERR_OPENAL_CTXCURRENT |
  MMERR_OPENAL_GENBUFFERS |
  MMERR_OPENAL_GENSOURCES |
  MMERR_OPENAL_SOURCE |
  MMERR_OPENAL_QUEUEBUFFERS |
  MMERR_OPENAL_UNQUEUEBUFFERS |
  MMERR_OPENAL_BUFFERDATA |
  MMERR_OPENAL_GETSOURCE |
  MMERR_OPENAL_SOURCEPLAY |
  MMERR_OPENAL_SOURCESTOP |

  MMERR_ALSA_NOCONFIG |
  MMERR_ALSA_SETPARAMS |
  MMERR_ALSA_SETFORMAT |
  MMERR_ALSA_SETRATE |
  MMERR_ALSA_SETCHANNELS |
  MMERR_ALSA_BUFFERSIZE |
  MMERR_ALSA_PCM_START |
  MMERR_ALSA_PCM_WRITE |
  MMERR_ALSA_PCM_RECOVER

