module Sound.MikMod.Synonyms where

import Foreign.C.Types

type UBYTE = CUChar
type SBYTE = CChar
type UWORD = CShort
type SWORD = CUShort
type ULONG = CUInt
type SLONG = CInt
type BOOL = CInt

decodeBool :: BOOL -> Bool
decodeBool 0 = False
decodeBool 1 = True
decodeBool x = error ("decodeBool " ++ show x)

encodeBool :: Num a => Bool -> a
encodeBool False = 0
encodeBool True  = 1
