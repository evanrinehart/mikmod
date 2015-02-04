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
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Data.Functor ((<$>))
import Control.Monad (when)
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

-- | To be returned by a readerGet if called at end-of-stream.
eof :: Int
eof = genericEof

genericEof :: Num a => a
genericEof = (#const EOF)

unmarshalSeekMode :: CInt -> SeekMode
unmarshalSeekMode n = case n of
  (#const SEEK_SET) -> AbsoluteSeek
  (#const SEEK_CUR) -> RelativeSeek
  (#const SEEK_END) -> SeekFromEnd

makeSeek :: (Int -> SeekMode -> IO Int) -> (Ptr () -> CLong -> CInt -> IO CInt)
makeSeek act _ offset whence = fromIntegral <$> act (fromIntegral offset) (unmarshalSeekMode whence)

makeTell :: IO Int -> (Ptr () -> IO CInt)
makeTell act _ = fromIntegral <$> act

makeRead :: (Ptr Word8 -> Int -> IO Bool) -> (Ptr () -> Ptr Word8 -> CSize -> IO BOOL)
makeRead act _ dest len = encodeBool <$> act dest (fromIntegral len)

makeGet :: IO Int -> (Ptr () -> IO CInt)
makeGet act _ = fromIntegral <$> act

makeEof :: IO Bool -> (Ptr () -> IO BOOL)
makeEof act _ = encodeBool <$> act

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

-- | Create an MReader from a ByteString. 
newByteStringReader :: ByteString -> IO MReader
newByteStringReader bs = do
  let len = BS.length bs
  rpos <- newIORef 0
  return $ MReader
    { readerTell = readIORef rpos
    , readerSeek = \n whence -> case whence of
        AbsoluteSeek -> writeIORef rpos n >> return 0
        RelativeSeek -> modifyIORef rpos (+n) >> return 0
        SeekFromEnd  -> writeIORef rpos (len - n) >> return 0
    , readerRead = \buf n -> do
        i <- readIORef rpos
        let i' = min (i+n) len
        let m = i' - i
        when (i < len) $ do
          writeIORef rpos i'
          let (payloadForeignPtr, off, _) = toForeignPtr bs
          withForeignPtr payloadForeignPtr
            (\payload -> memcpy buf (payload `plusPtr` (off+i)) (fromIntegral m))
        return True
    , readerGet = do
        i <- readIORef rpos
        if (i >= 0 && i < len)
          then do
            modifyIORef rpos (+1)
            return $ fromIntegral (BS.index bs i)
          else return eof
   ,  readerEof = do
        i <- readIORef rpos
        if (i >= 0 && i < len)
          then return False
          else return True
    }


-- | Wrap a Handle so it works like an MReader.
newHandleReader :: Handle -> MReader
newHandleReader h = MReader
  { readerSeek = \n whence -> do
      result <- try (hSeek h whence (fromIntegral n)) :: IO (Either IOError ())
      case result of
        Left _  -> return (-1)
        Right _ -> return 0
  , readerTell = fromIntegral <$> hTell h
  , readerRead = \to n -> do
      result <- try (BS.hGet h n) :: IO (Either IOError ByteString)
      case result of
        Left _   -> return False
        Right bs -> do
          if BS.null bs
            then return True
            else do
              let (payloadForeignPtr, off, len) = toForeignPtr bs
              withForeignPtr payloadForeignPtr
                (\from -> do memcpy to (from `plusPtr` off) (fromIntegral len))
              return True
  , readerGet = do
      bs <- BS.hGet h 1
      if BS.null bs
        then return eof
        else return (fromIntegral . BS.head $ bs)
  , readerEof = hIsEOF h
  }
