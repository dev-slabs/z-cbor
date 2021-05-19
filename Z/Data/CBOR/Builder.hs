{-# LANGUAGE NumericUnderscores #-}

{-|
Module    : Z.Data.CBOR.Builder
Description : CBOR builders
Copyright : (c) HanFei 2020
License   : BSD3
'Builder's to encode Haskell data types in CBOR format.
-}

module Z.Data.CBOR.Builder where

import           Control.Monad
import           Data.Bits
import           GHC.Int
import           Data.Word
import           Data.Primitive.PrimArray
import           GHC.Exts
import           GHC.Integer.GMP.Internals
import           Prelude                    hiding (map)
import           Z.Data.Array.Unaligned
import qualified Z.Data.Text                as T
import qualified Z.Data.Builder             as B
import qualified Z.Data.Vector              as V
import           Z.Data.CBOR.Value   hiding (value)

value :: Value -> B.Builder ()
{-# INLINABLE value #-}
value v = case v of
    Int    n -> int n
    Bin    b -> bin b
    Str    t -> str t
    Array  a -> array value a
    Map    m -> map value value m
    Tagged b r -> argument 0b110_00000 b >> value r
    Bool   b -> bool b
    Nil      -> nil
    Float  f -> float f
    Double d -> double d

int :: Int64 -> B.Builder ()
{-# INLINE int #-}
int n
    | n >= 0    = argument 0 (fromIntegral n)
    | otherwise = argument 0b001_00000 $ fromIntegral $ n *  (-1)

argument :: Word8 -> Word64 -> B.Builder ()
{-# INLINE argument #-}
argument majorType w
    | w < 24         =  B.encodePrim (tag (fromIntegral w) :: Word8)
    | w <= 0xFF       =  B.encodePrim (tag 24, fromIntegral w :: Word8)
    | w <= 0xFFFF      =  B.encodePrim (tag 25, BE (fromIntegral w :: Word16))
    | w <= 0xFFFFFFFF  =  B.encodePrim (tag 26, BE (fromIntegral w :: Word32))
    | otherwise        =  B.encodePrim (tag 27, BE (fromIntegral w :: Word64))
  where
    tag i = majorType .|. i

bool :: Bool -> B.Builder ()
{-# INLINE bool #-}
bool False = B.word8 0xF4
bool True  = B.word8 0xF5

nil :: B.Builder ()
{-# INLINE nil #-}
nil = B.word8 0xF6

float :: Float -> B.Builder ()
{-# INLINE float #-}
float f = B.encodePrim (0xFA :: Word8, BE f)

double :: Double -> B.Builder ()
{-# INLINE double #-}
double d = B.encodePrim (0xFB :: Word8, BE d)

str' :: String -> B.Builder ()
{-# INLINE str' #-}
str' = str . T.pack

str :: T.Text -> B.Builder ()
{-# INLINE str #-}
str t = do
    let bs = T.getUTF8Bytes t
    argument 0b011_00000 (fromIntegral $ V.length bs)
    B.bytes bs

bin :: V.Bytes -> B.Builder ()
{-# INLINE bin #-}
bin bs = do
    argument 0b010_00000 (fromIntegral $ V.length bs)
    B.bytes bs

array :: V.Vec v a => (a -> B.Builder ()) -> v a -> B.Builder ()
{-# INLINE array #-}
array p xs = do
    arrayHeader (V.length xs)
    V.traverseVec_ p xs

array' :: (a -> B.Builder ()) -> [a] -> B.Builder ()
{-# INLINE array' #-}
array' p xs = do
    arrayHeader (length xs)
    mapM_ p xs

arrayHeader :: Int -> B.Builder ()
{-# INLINE arrayHeader #-}
arrayHeader len = argument 0b100_00000 (fromIntegral len)

map :: (a -> B.Builder ()) -> (b -> B.Builder ()) -> V.Vector (a, b) -> B.Builder ()
{-# INLINE map #-}
map p q xs = do
    mapHeader (V.length xs)
    V.traverseVec_ (\(a, b) -> p a >> q b) xs

map' :: (a -> B.Builder ()) -> (b -> B.Builder ()) -> [(a, b)] -> B.Builder ()
{-# INLINE map' #-}
map' p q xs = do
    mapHeader (length xs)
    mapM_ (\(a, b) -> p a >> q b) xs

mapHeader :: Int -> B.Builder ()
{-# INLINE mapHeader #-}
mapHeader len = argument 0b101_00000 (fromIntegral len)
