{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Sound.MikMod.MReader where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal (allocaBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (toForeignPtr, memcpy)
import System.IO
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Functor ((<$>))
import Control.Monad (when, unless)
import Control.Exception (finally, try)

import Sound.MikMod.Synonyms
import Sound.MikMod.Types

#include <stdio.h>
#include <mikmod.h>

data MREADER

type SeekFn = Ptr () -> CLong -> CInt -> IO CInt
type TellFn = Ptr () -> IO CInt
type ReadFn = Ptr () -> Ptr Word8 -> CSize -> IO BOOL
type GetFn  = Ptr () -> IO CInt
type EofFn  = Ptr () -> IO BOOL

foreign import ccall "wrapper" mkSeek :: SeekFn -> IO (FunPtr SeekFn)
foreign import ccall "wrapper" mkTell :: TellFn -> IO (FunPtr TellFn)
foreign import ccall "wrapper" mkRead :: ReadFn -> IO (FunPtr ReadFn)
foreign import ccall "wrapper" mkGet  :: GetFn  -> IO (FunPtr GetFn)
foreign import ccall "wrapper" mkEof  :: EofFn  -> IO (FunPtr EofFn)

genericEof :: Num a => a
genericEof = (#const EOF)

unmarshalSeekMode :: CInt -> SeekMode
unmarshalSeekMode n = case n of
  (#const SEEK_SET) -> AbsoluteSeek
  (#const SEEK_CUR) -> RelativeSeek
  (#const SEEK_END) -> SeekFromEnd

makeSeek :: (Int -> SeekMode -> IO Outcome) -> (Ptr () -> CLong -> CInt -> IO CInt)
makeSeek act _ offset whence = do
  outcome <- act (fromIntegral offset) (unmarshalSeekMode whence)
  case outcome of
    Ok   -> return 0
    Fail -> return (-1)

makeTell :: IO Int -> (Ptr () -> IO CInt)
makeTell act _ = fromIntegral <$> act

makeRead :: (Int -> IO (Maybe ByteString)) -> (Ptr () -> Ptr Word8 -> CSize -> IO BOOL)
makeRead act _ dest csize = do
  let len = fromIntegral csize
  result <- act (fromIntegral len)
  case result of
    Nothing -> return 0
    Just bs -> do
      unsafeWriteByteStringToMemoryLocation (BS.take len bs) dest
      return 1

makeGet :: IO (Maybe Word8) -> (Ptr () -> IO CInt)
makeGet act _ = maybe genericEof fromIntegral <$> act

makeEof :: IO IsEOF -> (Ptr () -> IO BOOL)
makeEof act _ = do
  eof <- act
  case eof of
    EOF    -> return 1
    NotEOF -> return 0

-- | Allocate a MREADER and populate it with the correct function pointers.
-- Run the action on the MREADER and free it all even if an error occurs.
withMReader :: MReader -> (Ptr MREADER -> IO a) -> IO a
withMReader mr action = allocaBytes (#size MREADER) $ \ptr -> do
  fp1 <- mkSeek . makeSeek . readerSeek $ mr
  fp2 <- mkTell . makeTell . readerTell $ mr
  fp3 <- mkRead . makeRead . readerRead $ mr
  fp4 <- mkGet  . makeGet  . readerGet  $ mr
  fp5 <- mkEof  . makeEof  . readerEof  $ mr
  (#poke MREADER, Seek) ptr fp1
  (#poke MREADER, Tell) ptr fp2
  (#poke MREADER, Read) ptr fp3
  (#poke MREADER, Get ) ptr fp4
  (#poke MREADER, Eof ) ptr fp5
  finally (action ptr) $ do
    freeHaskellFunPtr fp1
    freeHaskellFunPtr fp2
    freeHaskellFunPtr fp3
    freeHaskellFunPtr fp4
    freeHaskellFunPtr fp5

-- | Make an MReader from a ByteString and a mutable variable for the
-- read position.
byteStringReader :: ByteString -> IORef Int -> MReader
byteStringReader bs rpos = let len = BS.length bs in MReader
  { readerSeek = \n whence -> do
      case whence of
        AbsoluteSeek -> writeIORef  rpos n
        RelativeSeek -> modifyIORef rpos (+n)
        SeekFromEnd  -> writeIORef  rpos (len - n)
      return Ok
  , readerTell = readIORef rpos
  , readerRead = \n -> do
      i <- readIORef rpos
      let i' = min (i+n) len
      let m = i' - i
      if i < len
        then do
          writeIORef rpos i'
          (return . Just . BS.take m . BS.drop i) bs
        else return (Just BS.empty)
  , readerGet = do
      i <- readIORef rpos
      if (i >= 0 && i < len)
        then do
          modifyIORef rpos (+1)
          (return . Just . fromIntegral) (BS.index bs i)
        else return Nothing
 ,  readerEof = do
      i <- readIORef rpos
      if (i >= 0 && i < len)
        then return NotEOF
        else return EOF
  }


-- | Wrap a Handle so it works like an MReader.
handleReader :: Handle -> MReader
handleReader h = MReader
  { readerSeek = \n whence -> do
      result <- try (hSeek h whence (fromIntegral n)) :: IO (Either IOError ())
      case result of
        Left _  -> return Fail
        Right _ -> return Ok
  , readerTell = fromIntegral <$> hTell h
  , readerRead = \n -> do
      result <- try (BS.hGet h n) :: IO (Either IOError ByteString)
      case result of
        Left _   -> return Nothing
        Right bs -> return (Just bs)
  , readerGet = do
      bs <- BS.hGet h 1
      if BS.null bs
        then return Nothing
        else (return . Just . fromIntegral . BS.head) bs
  , readerEof = do
      b <- hIsEOF h
      case b of
        True  -> return EOF
        False -> return NotEOF
  }


unsafeWriteByteStringToMemoryLocation :: ByteString -> Ptr Word8 -> IO ()
unsafeWriteByteStringToMemoryLocation bs dest =
  unless (BS.null bs) $ do
    let (fptr, offset, len) = toForeignPtr bs
    withForeignPtr fptr
      (\ptr -> memcpy dest (ptr `plusPtr` offset) (fromIntegral len))

