{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.MikMod.Module where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Data.Functor
import Control.Applicative

import Sound.MikMod.Synonyms
import Sound.MikMod.Types
import Sound.MikMod.Flags

#include <mikmod.h>

-- | Get a report of the static aspects of a module.
getModuleInfo :: ModuleHandle -> IO ModuleInfo
getModuleInfo ptr = ModuleInfo <$>
  (peekCStringError =<< ((#peek MODULE, songname) ptr :: IO CString)) <*>
  (peekCStringError =<< ((#peek MODULE, modtype) ptr :: IO CString)) <*>
  (peekCStringMaybe =<< ((#peek MODULE, comment) ptr :: IO CString)) <*>
  (unpackFlags <$> ((#peek MODULE, flags) ptr :: IO UWORD)) <*>
  (fromIntegral <$> ((#peek MODULE, numchn) ptr :: IO UBYTE)) <*>
  (fromIntegral <$> ((#peek MODULE, numvoices) ptr :: IO UBYTE)) <*>
  (fromIntegral <$> ((#peek MODULE, numpos) ptr :: IO UWORD)) <*>
  (fromIntegral <$> ((#peek MODULE, numpat) ptr :: IO UWORD)) <*>
  (fromIntegral <$> ((#peek MODULE, numins) ptr :: IO UWORD)) <*>
  (fromIntegral <$> ((#peek MODULE, numsmp) ptr :: IO UWORD)) <*>
  unpackInstrumentNames ptr

peekCStringMaybe :: CString -> IO (Maybe String)
peekCStringMaybe cstr | cstr == nullPtr = return Nothing
                      | otherwise = Just <$> peekCString cstr

peekCStringError :: CString -> IO String
peekCStringError cstr | cstr == nullPtr = error "peekCStringError NULL"
                      | otherwise = peekCString cstr

unpackInstrumentNames :: Ptr Module -> IO (Maybe [String])
unpackInstrumentNames mod = do
  n <- fromIntegral <$> ((#peek MODULE, numins) mod :: IO UWORD)
  ins0 <- (#peek MODULE, instruments) mod :: IO (Ptr Instrument)
  if (ins0 == nullPtr)
    then return Nothing
    else do
      let ptrs = map (\i -> ins0 `plusPtr` (sizeOfInstrument * i)) [0..n-1]
      Just <$> mapM getInstrumentName ptrs
  
getInstrumentName :: Ptr Instrument -> IO String
getInstrumentName ptr = peekCString =<< ((#peek INSTRUMENT, insname) ptr :: IO CString)

-- | Get handles to the samples contained in a module. I don't think it would
-- be wise to call sampleFree on these samples.
getModuleSamples :: ModuleHandle -> IO [SampleHandle]
getModuleSamples mod = do
  nsamps <- moduleNumSamples <$> getModuleInfo mod
  samps <- (#peek MODULE, samples) mod
  let stride = (#size SAMPLE)
  return (map (\i -> samps `plusPtr` (i*stride)) [0..nsamps-1])

sizeOfInstrument :: Int
sizeOfInstrument = (#size INSTRUMENT)

-- | During playback, the number of active channels (not counting NNA channels). 
getModuleRealChannels :: ModuleHandle -> IO Int
getModuleRealChannels mod = fromIntegral <$> ((#peek MODULE, realchn) mod :: IO UBYTE)

-- | During playback, the total number of channels (including NNA channels). 
getModuleTotalChannels :: ModuleHandle -> IO Int
getModuleTotalChannels mod = fromIntegral <$> ((#peek MODULE, totalchn) mod :: IO UBYTE)

-- | Elapsed song time in units of 1/1024 seconds. That's not milliseconds.
getModuleSongTime :: ModuleHandle -> IO Integer
getModuleSongTime mod = fromIntegral <$> ((#peek MODULE, sngtime) mod :: IO ULONG)

-- | Current song position.
getModuleSongPosition :: ModuleHandle -> IO Int
getModuleSongPosition mod = fromIntegral <$> ((#peek MODULE, sngpos) mod :: IO SWORD)

-- | Current position in the pattern being played.
getModulePatternPosition :: ModuleHandle -> IO Int
getModulePatternPosition mod = fromIntegral <$> ((#peek MODULE, patpos) mod :: IO UWORD)

-- | Set the initial speed of the module. Must be in range 1 - 32.
setModuleInitSpeed :: ModuleHandle -> Int -> IO ()
setModuleInitSpeed mod s = (#poke MODULE, initspeed) mod (fromIntegral s :: UBYTE)

getModuleInitSpeed :: ModuleHandle -> IO Int
getModuleInitSpeed mod = fromIntegral <$> ((#peek MODULE, initspeed) mod :: IO UBYTE)

-- | Set the initial tempo of the module. Must be in range 32 - 255.
setModuleInitTempo :: ModuleHandle -> Int -> IO ()
setModuleInitTempo mod temp = (#poke MODULE, inittempo) mod (fromIntegral temp :: UBYTE)

getModuleInitTempo :: ModuleHandle -> IO Int
getModuleInitTempo mod = fromIntegral <$> ((#peek MODULE, inittempo) mod :: IO UBYTE)

-- | Set the pan position of a channel in a module.
setModulePanning :: ModuleHandle
                 -> Int -- ^ Channel to set panning on.
                 -> Int -- ^ Pan position from 0 (far left) to 255 (far right).
                 -> IO ()
setModulePanning mod ch pan = do
  nchans <- moduleNumChannels <$> getModuleInfo mod
  chans <- (#peek MODULE, panning) mod :: IO (Ptr UWORD)
  if (ch >= 0 && ch < nchans)
    then pokeElemOff chans ch (fromIntegral pan)
    else return ()

-- | Query the pan position of a particular channel.
getModulePanning :: ModuleHandle -> Int -> IO Int
getModulePanning mod ch = do
  nchans <- moduleNumChannels <$> getModuleInfo mod
  chans <- (#peek MODULE, panning) mod :: IO (Ptr UWORD)
  if (ch >= 0 && ch < nchans)
    then fromIntegral <$> peekElemOff chans ch
    else return 0

-- | Set the volume of a channel in a module.
setModuleChannelVolume :: ModuleHandle
                       -> Int -- ^ Channel to set volume on.
                       -> Int -- ^ Volume level from 0 to 128.
                       -> IO ()
setModuleChannelVolume mod ch vol = do
  nchans <- moduleNumChannels <$> getModuleInfo mod
  chans <- (#peek MODULE, chanvol) mod :: IO (Ptr UBYTE)
  if (ch >= 0 && ch < nchans)
    then pokeElemOff chans ch (fromIntegral vol)
    else return ()

-- | Query the volume of a particular channel.
getModuleChannelVolume :: ModuleHandle -> Int -> IO Int
getModuleChannelVolume mod ch = do
  nchans <- moduleNumChannels <$> getModuleInfo mod
  chans <- (#peek MODULE, chanvol) mod :: IO (Ptr UBYTE)
  if (ch >= 0 && ch < nchans)
    then fromIntegral <$> peekElemOff chans ch
    else return 0

-- | Set the tempo of the module. See 'playerSetTempo'.
setModuleBPM :: ModuleHandle -> Int -> IO ()
setModuleBPM mod bpm = (#poke MODULE, bpm) mod (fromIntegral bpm :: UWORD)

getModuleBPM :: ModuleHandle -> IO Int
getModuleBPM mod = fromIntegral <$> ((#peek MODULE, bpm) mod :: IO UWORD)

-- | Set the speed of the module. See 'playerSetSpeed'.
setModuleSongSpeed :: ModuleHandle -> Int -> IO ()
setModuleSongSpeed mod spd = (#poke MODULE, sngspd) mod (fromIntegral spd :: UBYTE)

getModuleSongSpeed :: ModuleHandle -> IO Int
getModuleSongSpeed mod = fromIntegral <$> ((#peek MODULE, sngspd) mod :: IO UBYTE)

-- | Set the Protracker extended speed effect flag. True means process the
-- effect. Default is True.
setModuleExtSpeed :: ModuleHandle -> Bool -> IO ()
setModuleExtSpeed mod flag = (#poke MODULE, extspd) mod (encodeBool flag)

getModuleExtSpeed :: ModuleHandle -> IO Bool
getModuleExtSpeed mod = decodeBool <$> (#peek MODULE, extspd) mod

-- | Set the pan flag. True means process pan effects. Default is True.
setModulePanFlag :: ModuleHandle -> Bool -> IO ()
setModulePanFlag mod flag = (#poke MODULE, panflag) mod (encodeBool flag)

getModulePanFlag :: ModuleHandle -> IO Bool
getModulePanFlag mod = decodeBool <$> (#peek MODULE, panflag) mod

-- | Set the wrap flag. True means repeat from restart position at end of
-- song. Default is False, song ends.
setModuleWrap :: ModuleHandle -> Bool -> IO ()
setModuleWrap mod flag = (#poke MODULE, wrap) mod (encodeBool flag)

getModuleWrap :: ModuleHandle -> IO Bool
getModuleWrap mod = decodeBool <$> (#peek MODULE, wrap) mod

-- | Set the restart position.
setModuleRepeatPosition :: ModuleHandle -> Int -> IO ()
setModuleRepeatPosition mod pos = (#poke MODULE, reppos) mod (fromIntegral pos :: UBYTE)

getModuleRepeatPosition :: ModuleHandle -> IO Int
getModuleRepeatPosition mod = fromIntegral <$> ((#peek MODULE, reppos) mod :: IO UBYTE)

-- | Set the loop flag. False means only process forward loops or same-pattern
-- backward loops. Default is True.
setModuleLoop :: ModuleHandle -> Bool -> IO ()
setModuleLoop mod flag = (#poke MODULE, loop) mod (encodeBool flag :: BOOL)

getModuleLoop :: ModuleHandle -> IO Bool
getModuleLoop mod = decodeBool <$> (#peek MODULE, loop) mod

-- | Set the fadeout flag of the module. True means fade out. Default is False.
setModuleFadeout :: ModuleHandle -> Bool -> IO ()
setModuleFadeout mod flag = (#poke MODULE, fadeout) mod (encodeBool flag)

getModuleFadeout :: ModuleHandle -> IO Bool
getModuleFadeout mod = decodeBool <$> ((#peek MODULE, fadeout) mod :: IO BOOL)

-- | This value is added to the module tempo to define actual playback speed.
-- Default is zero.
setModuleRelativeSpeed :: ModuleHandle -> Int -> IO ()
setModuleRelativeSpeed mod s = (#poke MODULE, relspd) mod (fromIntegral s :: SWORD)

getModuleRelativeSpeed :: ModuleHandle -> IO Int
getModuleRelativeSpeed mod = fromIntegral <$> ((#peek MODULE, relspd) mod :: IO SWORD)

