{-# LANGUAGE NumericUnderscores #-}

{- |
Module    : Z.Data.CBOR.Value
Description : CBOR object definition and parser
Copyright : (c) HanFei 2020
License   : BSD3
-}
module Z.Data.CBOR.Value (
  -- * CBOR Value
    Value(..)
    -- * parse into CBOR Value
  , parseValue
  , parseValue'
    -- * Value Parsers
  , value

) where

import           Control.DeepSeq
import           Control.Monad
import           Data.Bits
import           Data.Int
import           Data.Word
import           GHC.Generics               (Generic)
import           Test.QuickCheck.Arbitrary  (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen        as Gen
import           Prelude                    hiding (map)
import qualified Z.Data.Text                as T
import qualified Z.Data.Parser              as P
import qualified Z.Data.Vector              as V

-- | Representation of CBOR data.
data Value
    = Int    {-# UNPACK #-} !Int64                  -- ^ type 0/1, an integer
    | Bin    {-# UNPACK #-} !V.Bytes                -- ^ type 2, a byte array
    | Str    {-# UNPACK #-} !T.Text                 -- ^ type 3, a UTF-8 string
    | Array  {-# UNPACK #-} !(V.Vector Value)       -- ^ type 4, a sequence of objects
    | Map    {-# UNPACK #-} !(V.Vector (Value, Value)) -- ^ type 5, key-value pairs of objects
    | Tagged {-# UNPACK #-} !Word64                  -- ^ type 6, tagged item
             {-# UNPACK #-} !Value                -- ^ data payload
    -- simple value
    | Bool                  !Bool                   -- ^ true or false
    | Nil                                           -- ^ nil
    | Float  {-# UNPACK #-} !Float                  -- ^ a floating point number
    | Double {-# UNPACK #-} !Double                 -- ^ a floating point number
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass T.Print

instance NFData Value where
    rnf obj = case obj of
        Array a -> rnf a
        Map   m -> rnf m
        _             -> ()

instance Arbitrary Value where
    arbitrary = Gen.sized $ \n -> Gen.oneof
        [ Bool   <$> arbitrary
        , Int    <$> negatives
        , Float  <$> arbitrary
        , Double <$> arbitrary
        , Str    <$> arbitrary
        , Bin    <$> arbitrary
        , Array  <$> Gen.resize (n `div` 2) arbitrary
        , Map    <$> Gen.resize (n `div` 4) arbitrary
        , Tagged <$> arbitrary <*> arbitrary
        , pure Nil
        ]
        where negatives = Gen.choose (minBound, -1)


value :: P.Parser Value
{-# INLINABLE value #-}
value = do
    tag <- P.anyWord8
    case tag `shiftR` 5 of
        -- unsigned integer
        0 -> Int . fromIntegral <$> argument tag
        -- negative integer
        1 -> Int . (* (-1)) . fromIntegral <$> argument tag
        -- Binary
        2 -> bin =<< argument tag
        -- String
        3 -> str =<< argument tag
        -- Array
        4 -> array =<< argument tag
        -- Map
        5 -> map =<< argument tag
        -- Tag
        6 -> Tagged <$> argument tag <*> value
        _ -> case typeInfo tag of
          -- Bool
          20 -> return (Bool False)
          21 -> return (Bool True)
          -- Null
          22 -> return Nil
          -- Float
          26 -> Float <$> P.decodePrimBE @Float
          -- Double
          27 -> Double <$> P.decodePrimBE @Double
          -- impossible
          _ -> P.fail' ("Z.Data.CBOR: unknown tag " <> T.toText tag)

  where
    typeInfo :: Word8 -> Word8
    typeInfo w = w .&. 0b000_11111

    argument :: Word8 -> P.Parser Word64
    argument (typeInfo -> i)
      | i < 24 = pure (fromIntegral i)
      | i == 24 = fromIntegral <$> P.anyWord8
      | i == 25 = fromIntegral <$> P.decodePrimBE @Word16
      | i == 26 = fromIntegral <$> P.decodePrimBE @Word32
      | i == 27 = fromIntegral <$> P.decodePrimBE @Word64
      | otherwise = P.fail' ("Z.Data.CBOR: unknown additional information " <> T.toText i)

    str !l = do
        bs <- P.take (fromIntegral l)
        case T.validateMaybe bs of
            Just t -> return (Str t)
            _  -> P.fail' "Z.Data.CBOR: illegal UTF8 Bytes"
    bin !l   = Bin <$> P.take (fromIntegral l)
    array !l = Array . V.packN (fromIntegral l) <$> replicateM (fromIntegral l) value
    map !l   = Map . V.packN (fromIntegral l) <$> replicateM (fromIntegral l) ((,) <$> value <*> value)

-- | Parse 'Value' without consuming trailing bytes.
parseValue :: V.Bytes -> (V.Bytes, Either P.ParseError Value)
{-# INLINE parseValue #-}
parseValue = P.parse value

-- | Parse 'Value', if there're bytes left, parsing will fail.
parseValue' :: V.Bytes -> Either P.ParseError Value
{-# INLINE parseValue' #-}
parseValue' = P.parse' (value <* P.endOfInput)