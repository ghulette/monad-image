module Color where

import Data.Bits
import Data.Word

data Color = Color !Word8 !Word8 !Word8

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = Color r g b

red :: Color -> Word8
red (Color r _ _) = r

green :: Color -> Word8
green (Color _ g _) = g

blue :: Color -> Word8
blue (Color _ _ b) = b

pack :: Color -> Word32
pack (Color r g b) =  (fromIntegral r) 
                  .|. (fromIntegral g) `shiftL` 8 
                  .|. (fromIntegral b) `shiftL` 16

unpack :: Word32 -> Color
unpack w = Color r g b
  where r = fromIntegral w
        g = (fromIntegral w) `shiftR` 8
        b = (fromIntegral w) `shiftR` 16
