{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.MikMod.Internal where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Functor
import Control.Applicative
import System.IO.Unsafe
import Control.Exception

import Sound.MikMod.Synonyms
import Sound.MikMod.Types
import Sound.MikMod.Flags
import Sound.MikMod.Errors
import Sound.MikMod.MReader

#include "mikmod.h"

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

foreign import ccall safe "mikmod.h MikMod_Active" c_MikMod_Active :: IO BOOL
foreign import ccall safe "mikmod.h MikMod_DisableOutput" c_MikMod_DisableOutput :: IO ()
foreign import ccall safe "mikmod.h MikMod_EnableOutput" c_MikMod_EnableOutput :: IO ()
foreign import ccall safe "mikmod.h MikMod_Exit" c_MikMod_Exit :: IO ()
foreign import ccall safe "mikmod.h MikMod_GetVersion" c_MikMod_GetVersion :: IO CInt
foreign import ccall safe "mikmod.h MikMod_InfoDriver" c_MikMod_InfoDriver :: IO CString
foreign import ccall safe "mikmod.h MikMod_InfoLoader" c_MikMod_InfoLoader :: IO CString
foreign import ccall safe "mikmod.h MikMod_Init" c_MikMod_Init :: CString -> IO CInt
foreign import ccall safe "mikmod.h MikMod_InitThreads" c_MikMod_InitThreads :: IO CInt
foreign import ccall safe "mikmod.h MikMod_Lock" c_MikMod_Lock :: IO ()
foreign import ccall safe "mikmod.h MikMod_Unlock" c_MikMod_Unlock :: IO ()
foreign import ccall safe "mikmod.h MikMod_RegisterAllDrivers" c_MikMod_RegisterAllDrivers :: IO ()
foreign import ccall safe "mikmod.h MikMod_RegisterAllLoaders" c_MikMod_RegisterAllLoaders :: IO ()
foreign import ccall safe "mikmod.h MikMod_Reset" c_MikMod_Reset :: CString -> IO CInt
foreign import ccall safe "mikmod.h MikMod_SetNumVoices" c_MikMod_SetNumVoices :: CInt -> CInt -> IO CInt
foreign import ccall safe "mikmod.h MikMod_Update" c_MikMod_Update :: IO ()
foreign import ccall safe "mikmod.h MikMod_strerror" c_MikMod_strerror :: CInt -> IO CString

foreign import ccall safe "mikmod.h Player_Active" c_Player_Active :: IO CInt
foreign import ccall safe "mikmod.h Player_Free" c_Player_Free :: Ptr Module -> IO ()
foreign import ccall safe "mikmod.h Player_GetChannelVoice" c_Player_GetChannelVoice :: UBYTE -> IO SBYTE
foreign import ccall safe "mikmod.h Player_GetModule" c_Player_GetModule :: IO (Ptr Module)
foreign import ccall safe "mikmod.h Player_Load" c_Player_Load :: CString -> CInt -> CInt -> IO (Ptr Module)
foreign import ccall safe "mikmod.h Player_LoadGeneric" c_Player_LoadGeneric :: Ptr MREADER -> CInt -> CInt -> IO (Ptr Module)
foreign import ccall safe "mikmod.h Player_LoadTitle" c_Player_LoadTitle :: CString -> IO CString
foreign import ccall safe "mikmod.h Player_LoadTitleGeneric" c_Player_LoadTitleGeneric :: Ptr MREADER -> IO CString
foreign import ccall safe "mikmod.h Player_Mute" c_Player_MuteChannel :: CInt -> IO ()
foreign import ccall safe "mikmod.h Player_Mute" c_Player_MuteChannels :: CInt -> CInt -> CInt -> IO ()
foreign import ccall safe "mikmod.h Player_Muted" c_Player_Muted :: UBYTE -> IO CInt
foreign import ccall safe "mikmod.h Player_NextPosition" c_Player_NextPosition :: IO ()
foreign import ccall safe "mikmod.h Player_PrevPosition" c_Player_PrevPosition :: IO ()
foreign import ccall safe "mikmod.h Player_Paused" c_Player_Paused :: IO CInt
foreign import ccall safe "mikmod.h Player_SetPosition" c_Player_SetPosition :: UWORD -> IO ()
foreign import ccall safe "mikmod.h Player_SetSpeed" c_Player_SetSpeed :: UWORD -> IO ()
foreign import ccall safe "mikmod.h Player_SetTempo" c_Player_SetTempo :: UWORD -> IO ()
foreign import ccall safe "mikmod.h Player_SetVolume" c_Player_SetVolume :: SWORD -> IO ()
foreign import ccall safe "mikmod.h Player_Start" c_Player_Start :: Ptr Module -> IO ()
foreign import ccall safe "mikmod.h Player_Stop" c_Player_Stop :: IO ()
foreign import ccall safe "mikmod.h Player_ToggleMute" c_Player_ToggleMuteChannel :: CInt -> IO ()
foreign import ccall safe "mikmod.h Player_ToggleMute" c_Player_ToggleMuteChannels :: CInt -> CInt -> CInt -> IO ()
foreign import ccall safe "mikmod.h Player_TogglePause" c_Player_TogglePause :: IO ()
foreign import ccall safe "mikmod.h Player_Unmute" c_Player_UnmuteChannel :: CInt -> IO ()
foreign import ccall safe "mikmod.h Player_Unmute" c_Player_UnmuteChannels :: CInt -> CInt -> CInt -> IO ()

foreign import ccall safe "mikmod.h Sample_Free" c_Sample_Free :: Ptr Sample -> IO ()
foreign import ccall safe "mikmod.h Sample_Load" c_Sample_Load :: CString -> IO (Ptr Sample)
foreign import ccall safe "mikmod.h Sample_LoadGeneric" c_Sample_LoadGeneric :: Ptr MREADER -> IO (Ptr Sample)
foreign import ccall safe "mikmod.h Sample_Play" c_Sample_Play :: Ptr Sample -> ULONG -> UBYTE -> IO SBYTE

foreign import ccall safe "mikmod.h Voice_SetVolume" c_Voice_SetVolume :: SBYTE -> UWORD -> IO ()
foreign import ccall safe "mikmod.h Voice_GetVolume" c_Voice_GetVolume :: SBYTE -> IO UWORD
foreign import ccall safe "mikmod.h Voice_SetFrequency" c_Voice_SetFrequency :: SBYTE -> ULONG -> IO ()
foreign import ccall safe "mikmod.h Voice_GetFrequency" c_Voice_GetFrequency :: SBYTE -> IO ULONG
foreign import ccall safe "mikmod.h Voice_SetPanning" c_Voice_SetPanning :: SBYTE -> ULONG -> IO ()
foreign import ccall safe "mikmod.h Voice_GetPanning" c_Voice_GetPanning :: SBYTE -> IO ULONG
foreign import ccall safe "mikmod.h Voice_Play" c_Voice_Play :: SBYTE -> Ptr Sample -> ULONG -> IO ()
foreign import ccall safe "mikmod.h Voice_Stop" c_Voice_Stop :: SBYTE -> IO ()
foreign import ccall safe "mikmod.h Voice_Stopped" c_Voice_Stopped :: SBYTE -> IO BOOL
foreign import ccall safe "mikmod.h Voice_GetPosition" c_Voice_GetPosition :: SBYTE -> IO SLONG
foreign import ccall safe "mikmod.h Voice_RealVolume" c_Voice_RealVolume :: SBYTE -> IO ULONG

foreign import ccall safe "mikmod.h MikMod_free" c_MikMod_free :: Ptr a -> IO ()

marshalMuteOperation :: MuteOperation -> CInt
marshalMuteOperation MuteInclusive = (#const MUTE_INCLUSIVE)
marshalMuteOperation MuteExclusive = (#const MUTE_EXCLUSIVE)

peekMDriver :: Ptr MDriver -> IO MDriverInfo
peekMDriver ptr = do
  name  <- peekCString =<< (peek ((#ptr MDRIVER, Name) ptr) :: IO CString)
  alias <- peekCString =<< (peek ((#ptr MDRIVER, Alias) ptr) :: IO CString)
  hard  <- fromIntegral <$> (peek ((#ptr MDRIVER, HardVoiceLimit) ptr) :: IO CUChar)
  soft  <- fromIntegral <$> (peek ((#ptr MDRIVER, SoftVoiceLimit) ptr) :: IO CUChar)
  return $ MDriverInfo name hard soft alias


-- | Query the current MikMod global errno and get the MikModError expressed
-- there, if any. This value is only valid if checked immediately after an
-- error occurs. If you are interested in MikModErrors use the ""Safe"" versions
-- of the API methods which return an Either MikModError.
mikmodGetError :: IO MikModError
mikmodGetError = do
  errno <- unmarshalMikModErrno <$> peek c_MikMod_errno
  crit  <- decodeBool <$> peek c_MikMod_critical
  if crit
    then return (Critical errno)
    else return (NonCritical errno)

sfxCritical :: UBYTE
sfxCritical = (#const SFX_CRITICAL)

marshalCurious :: Num a => CuriousFlag -> a
marshalCurious Curious = 1
marshalCurious NotCurious = 0

mikmodGetString :: IO CString -> IO (Maybe String)
mikmodGetString query = do
  ptr <- query
  if (ptr == nullPtr)
    then return Nothing
    else
      Just <$> peekCString ptr
      `finally`
      c_MikMod_free ptr

