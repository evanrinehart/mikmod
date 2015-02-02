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

-- | Get a report of the current state of a sample.
getSampleInfo :: SampleHandle -> IO SampleInfo
getSampleInfo ptr = SampleInfo <$>
  (fromIntegral <$> (peek $ (#ptr SAMPLE, panning) ptr :: IO SWORD)) <*>
  (fromIntegral <$> (peek $ (#ptr SAMPLE, speed) ptr :: IO ULONG)) <*>
  (fromIntegral <$> (peek $ (#ptr SAMPLE, volume) ptr :: IO UBYTE)) <*>
  (unpackFlags <$> (peek $ (#ptr SAMPLE, flags) ptr :: IO UWORD)) <*>
  (unpackFlags <$> (peek $ (#ptr SAMPLE, inflags) ptr :: IO UWORD)) <*>
  (fromIntegral <$> (peek $ (#ptr SAMPLE, length) ptr :: IO ULONG)) <*>
  (fromIntegral <$> (peek $ (#ptr SAMPLE, loopstart) ptr :: IO ULONG)) <*>
  (fromIntegral <$> (peek $ (#ptr SAMPLE, loopend) ptr :: IO ULONG))

setSamplePanning :: SampleHandle -> Int -> IO ()
setSamplePanning samp pan = undefined

getSamplePanning :: SampleHandle -> IO Int
getSamplePanning = undefined

setSampleSpeed :: SampleHandle -> Int -> IO ()
setSampleSpeed = undefined

getSampleSpeed :: SampleHandle -> IO Int
getSampleSpeed = undefined

setSampleVolume :: SampleHandle -> Int -> IO ()
setSampleVolume samp vol = (#poke SAMPLE, volume) samp (fromIntegral vol :: UBYTE)

getSampleVolume :: SampleHandle -> IO Int
getSampleVolume = undefined

setSampleFlags :: SampleHandle -> Int -> IO ()
setSampleFlags = undefined

getSampleFlags :: SampleHandle -> IO Int
getSampleFlags = undefined

getSampleInFlags :: SampleHandle -> IO Int
getSampleInFlags = undefined

getSampleLength :: SampleHandle -> IO Int
getSampleLength = undefined

setSampleLoopStart :: SampleHandle -> Int -> IO ()
setSampleLoopStart = undefined

getSampleLoopStart :: SampleHandle -> IO Int
getSampleLoopStart = undefined

setSampleLoopEnd :: SampleHandle -> Int -> IO ()
setSampleLoopEnd = undefined

getSampleLoopEnd :: SampleHandle -> IO Int
getSampleLoopEnd = undefined

