{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.MikMod.Sample where

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

-- | Far left pan. panLeft = Pan 0.
panLeft :: Pan
panLeft = Pan $ fromIntegral (#const PAN_LEFT)

-- | Far right pan. panRight = Pan 255.
panRight :: Pan
panRight = Pan $ fromIntegral (#const PAN_RIGHT)

packPan :: Pan -> SWORD
packPan (Pan n) = fromIntegral n
packPan PanSurround = (#const PAN_SURROUND)

unpackPan :: SWORD -> Pan
unpackPan n | n == (#const PAN_SURROUND) = PanSurround
            | n >= 0 && n <= 255 = Pan (fromIntegral n)
            | otherwise = error ("unpackPan " ++ show n)

-- | Get a report of the current state of a sample.
getSampleInfo :: SampleHandle -> IO SampleInfo
getSampleInfo ptr = SampleInfo <$>
  (unpackPan    <$> (#peek SAMPLE, panning) ptr) <*>
  (fromIntegral <$> ((#peek SAMPLE, speed) ptr :: IO ULONG)) <*>
  (fromIntegral <$> ((#peek SAMPLE, volume) ptr :: IO UBYTE)) <*>
  (unpackFlags  <$> (#peek SAMPLE, flags) ptr) <*>
  (unpackFlags  <$> (#peek SAMPLE, inflags) ptr) <*>
  (fromIntegral <$> ((#peek SAMPLE, length) ptr :: IO ULONG)) <*>
  (fromIntegral <$> ((#peek SAMPLE, loopstart) ptr :: IO ULONG)) <*>
  (fromIntegral <$> ((#peek SAMPLE, loopend) ptr :: IO ULONG))

-- | Set the panning value of the sample. Valid values range from panLeft (0)
-- to panRight (255), or PanSurround. 
setSamplePanning :: SampleHandle -> Pan -> IO ()
setSamplePanning samp pan = (#poke SAMPLE, panning) samp (packPan pan)

getSamplePanning :: SampleHandle -> IO Pan
getSamplePanning samp = unpackPan <$> (#peek SAMPLE, panning) samp

-- | Set the sample playing frequency in Hertz.
setSampleSpeed :: SampleHandle -> Int -> IO ()
setSampleSpeed samp speed = (#poke SAMPLE, speed) samp (fromIntegral speed :: ULONG)

getSampleSpeed :: SampleHandle -> IO Int
getSampleSpeed samp = fromIntegral <$> ((#peek SAMPLE, speed) samp :: IO ULONG)

-- | Set sample volume to a range 0 to 64 (65 levels). 
setSampleVolume :: SampleHandle -> Int -> IO ()
setSampleVolume samp vol = (#poke SAMPLE, volume) samp (fromIntegral vol :: UBYTE)

getSampleVolume :: SampleHandle -> IO Int
getSampleVolume samp = fromIntegral <$> ((#peek SAMPLE, volume) samp :: IO UBYTE)

-- | Set the sample flags. Useful for setting the loop, reverse, and bi-directional
-- playback characteristics of the sample.
setSampleFlags :: SampleHandle -> [SampleFlag] -> IO ()
setSampleFlags samp flags = (#poke SAMPLE, flags) samp (packFlags flags)

getSampleFlags :: SampleHandle -> IO [SampleFlag]
getSampleFlags samp = unpackFlags <$> (#peek SAMPLE, flags) samp

-- | Query the "on disk" flags of the sample if you're curious about the
-- original format of the sample.
getSampleInFlags :: SampleHandle -> IO [SampleFlag]
getSampleInFlags samp = unpackFlags <$> (#peek SAMPLE, inflags) samp

-- | Query the length of the sample... in samples. The byte-size of a (big) sample
-- is related to the size of a (small) sample in bytes, either 8bit or 16bit.
getSampleLength :: SampleHandle -> IO Int
getSampleLength samp = fromIntegral <$> ((#peek SAMPLE, length) samp :: IO ULONG)

-- | Set the loop starting position in samples.
setSampleLoopStart :: SampleHandle -> Int -> IO ()
setSampleLoopStart samp start = (#poke SAMPLE, loopstart) samp (fromIntegral start :: ULONG)

getSampleLoopStart :: SampleHandle -> IO Int
getSampleLoopStart samp = fromIntegral <$> ((#peek SAMPLE, loopstart) samp :: IO ULONG)

-- | Set the loop end position in samples.
setSampleLoopEnd :: SampleHandle -> Int -> IO ()
setSampleLoopEnd samp end = (#poke SAMPLE, loopend) samp (fromIntegral end :: ULONG)

getSampleLoopEnd :: SampleHandle -> IO Int
getSampleLoopEnd samp = fromIntegral <$> ((#peek SAMPLE, loopend) samp :: IO ULONG)

