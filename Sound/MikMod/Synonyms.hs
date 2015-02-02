module Sound.MikMod.Synonyms where

import Foreign.C.Types

type UBYTE = CUChar
type SBYTE = CChar
type UWORD = CUShort
type SWORD = CShort
type ULONG = CUInt
type SLONG = CInt
type BOOL = CInt

decodeBool :: BOOL -> Bool
decodeBool 0 = False
decodeBool 1 = True
decodeBool x = error ("decodeBool " ++ show x)

encodeBool :: Bool -> BOOL
encodeBool False = 0
encodeBool True  = 1

genericEncodeBool :: Num a => Bool -> a
genericEncodeBool x = if x then 1 else 0
