{-|
Module      : Z.Data.CBOR.Base
Description : Fast CBOR serialization/deserialization
Copyright   : (c) Dong Han, 2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable
This module provides various tools to help user define 'CBOR' instance, please import `Z.Data.CBOR` to get more instances.
-}

module Z.Data.CBOR.Base
  ( -- * CBOR Class
    CBOR(..), Value(..), defaultSettings, Settings(..)
    -- * Encode & Decode
  , decode, decode', decodeChunk, decodeChunks, encode, encodeChunks
  , DecodeError, P.ParseError, P.ParseChunks
    -- * parse into CBOR Value
  , CV.parseValue, CV.parseValue'
  -- * Generic FromValue, ToValue & EncodeCBOR
  , gToValue, gFromValue, gEncodeCBOR
  -- * Convert 'Value' to Haskell data
  , convertValue, Converter(..), fail', (<?>), prependContext
  , PathElement(..), ConvertError(..)
  , typeMismatch, fromNil, withBool
  , withStr, withBin, withArray, withKeyValues, withFlatMap, withFlatMapR
  , (.:), (.:?), (.:!), convertField, convertFieldMaybe, convertFieldMaybe'
  -- * Helper for manually writing instance.
  , (.=), object, (.!), object', KVItem
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char                      (ord)
import           Data.Data
import           Data.Fixed
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum
import qualified Data.Foldable                  as Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.Map.Strict                as M
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as Set
import qualified Data.Tree                      as Tree
import           GHC.Int
import           GHC.Exts
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.List.NonEmpty             as NonEmpty
import qualified Data.Monoid                    as Monoid
import qualified Data.Primitive.ByteArray       as A
import qualified Data.Primitive.SmallArray      as A
import           Data.Primitive.Types           (Prim)
import           Data.Proxy                     (Proxy (..))
import           Data.Ratio                     (Ratio, denominator, numerator, (%))
import           Data.Scientific                (Scientific, coefficient, base10Exponent)
import qualified Data.Scientific                as Sci
import qualified Data.Semigroup                 as Semigroup
-- import           Data.Tagged                    (Tagged (..))
import           Data.Time                      (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import           Data.Time.Calendar             (CalendarDiffDays (..), DayOfWeek (..))
import           Data.Time.LocalTime            (CalendarDiffTime (..))
import           Data.Time.Clock.System         (SystemTime (..), systemToUTCTime, utcToSystemTime)
import           Data.Version                   (Version, parseVersion)
import           Data.Word
import           Foreign.C.Types
import           GHC.Exts                       (Proxy#, proxy#)
import           GHC.Generics
import           GHC.Natural
import           GHC.Integer.GMP.Internals
import           System.Exit
import           Text.ParserCombinators.ReadP   (readP_to_S)
import qualified Z.Data.Array                   as A
import qualified Z.Data.Builder                 as B
import qualified Z.Data.CBytes                  as CBytes
import           Z.Data.Generics.Utils
import           Z.Data.JSON.Converter
import qualified Z.Data.CBOR.Builder     as CB
import           Z.Data.CBOR.Value       (Value (..))
import qualified Z.Data.CBOR.Value       as CV
import qualified Z.Data.Parser                  as P
import qualified Z.Data.Parser.Numeric          as P
import qualified Z.Data.Text.Base               as T
import qualified Z.Data.Text                    as T
import qualified Z.Data.Text.Print              as T
import qualified Z.Data.Vector.Base             as V
import qualified Z.Data.Vector.Extra            as V
import qualified Z.Data.Vector.FlatIntMap       as FIM
import qualified Z.Data.Vector.FlatIntSet       as FIS
import qualified Z.Data.Vector.FlatMap          as FM
import qualified Z.Data.Vector.FlatSet          as FS

--------------------------------------------------------------------------------

-- | Type class for encode & decode CBOR.
class CBOR a where
    fromValue :: Value -> Converter a
    default fromValue :: (Generic a, GFromValue (Rep a)) => Value -> Converter a
    fromValue v = to <$> gFromValue defaultSettings v
    {-# INLINABLE fromValue #-}

    toValue :: a -> Value
    default toValue :: (Generic a, GToValue (Rep a)) => a -> Value
    toValue = gToValue defaultSettings . from
    {-# INLINABLE toValue #-}

    encodeCBOR :: a -> B.Builder ()
    default encodeCBOR :: (Generic a, GEncodeCBOR (Rep a)) => a -> B.Builder ()
    encodeCBOR = gEncodeCBOR defaultSettings . from
    {-# INLINABLE encodeCBOR #-}

--------------------------------------------------------------------------------

-- There're two possible failures here:
--
--   * 'P.ParseError' is an error during parsing bytes to 'Value'.
--   * 'ConvertError' is an error when converting 'Value' to target data type.
type DecodeError = Either P.ParseError ConvertError

-- | Decode a CBOR doc, trailing bytes are not allowed.
decode' :: CBOR a => V.Bytes -> Either DecodeError a
{-# INLINE decode' #-}
decode' bs = case P.parse' (CV.value <* P.endOfInput) bs of
    Left pErr -> Left (Left pErr)
    Right v -> case convertValue v of
        Left cErr -> Left (Right cErr)
        Right r   -> Right r

-- | Decode a CBOR bytes, return any trailing bytes.
decode :: CBOR a => V.Bytes -> (V.Bytes, Either DecodeError a)
{-# INLINE decode #-}
decode bs = case P.parse CV.value bs of
    (bs', Left pErr) -> (bs', Left (Left pErr))
    (bs', Right v) -> case convertValue v of
        Left cErr -> (bs', Left (Right cErr))
        Right r   -> (bs', Right r)

-- | Decode a CBOR doc chunk.
decodeChunk :: CBOR a => V.Bytes -> P.Result DecodeError a
{-# INLINE decodeChunk #-}
decodeChunk bs = loop (P.parseChunk CV.value bs)
  where
    loop r = do
        case r of
            P.Success v rest ->
                case convertValue v of
                    Left cErr -> P.Failure (Right cErr) rest
                    Right r'  -> P.Success r' rest
            P.Failure e rest -> P.Failure (Left e) rest
            P.Partial f' -> P.Partial (loop . f')

-- | Decode CBOR doc chunks, return trailing bytes.
decodeChunks :: (CBOR a, Monad m) => P.ParseChunks m DecodeError a
{-# INLINE decodeChunks #-}
decodeChunks = P.parseChunks decodeChunk

-- | Directly encode data to CBOR bytes.
encode :: CBOR a => a -> V.Bytes
{-# INLINE encode #-}
encode = B.build . encodeCBOR

-- | Encode data to CBOR bytes chunks.
encodeChunks :: CBOR a => a -> [V.Bytes]
{-# INLINE encodeChunks #-}
encodeChunks = B.buildChunks . encodeCBOR

-- | Run a 'Converter' with input value.
convertValue :: (CBOR a) => Value -> Either ConvertError a
{-# INLINE convertValue #-}
convertValue = convert fromValue

--------------------------------------------------------------------------------

-- | Produce an error message like @converting XXX failed, expected XXX, encountered XXX@.
typeMismatch :: T.Text     -- ^ The name of the type you are trying to convert.
             -> T.Text     -- ^ The CBOR value type you expecting to meet.
             -> Value      -- ^ The actual value encountered.
             -> Converter a
{-# INLINE typeMismatch #-}
typeMismatch name expected v =
    fail' $ T.concat ["converting ", name, " failed, expected ", expected, ", encountered ", actual]
  where
    actual = case v of
        Nil      ->  "Nil"
        Bool _   ->  "Bool"
        Int  _   ->  "Int"
        Float _  ->  "Float"
        Double _ ->  "Double"
        Str _    ->  "Str"
        Bin _    ->  "Bin"
        Array _  ->  "Array"
        Map _    ->  "Map"
        Tagged _ _  ->  "Tagged"

fromNil :: T.Text -> a -> Value -> Converter a
{-# INLINE fromNil #-}
fromNil _ a Nil = pure a
fromNil c _ v    = typeMismatch c "Nil" v

withBool :: T.Text -> (Bool -> Converter a) -> Value ->  Converter a
{-# INLINE withBool #-}
withBool _    f (Bool x) = f x
withBool name _ v        = typeMismatch name "Bool" v

withStr :: T.Text -> (T.Text -> Converter a) -> Value -> Converter a
{-# INLINE withStr #-}
withStr _    f (Str x) = f x
withStr name _ v       = typeMismatch name "Str" v

withBin :: T.Text -> (V.Bytes -> Converter a) -> Value -> Converter a
{-# INLINE withBin #-}
withBin _    f (Bin x) = f x
withBin name _ v       = typeMismatch name "Bin" v

withArray :: T.Text -> (V.Vector Value -> Converter a) -> Value -> Converter a
{-# INLINE withArray #-}
withArray _ f (Array arr) = f arr
withArray name _ v      = typeMismatch name "Arr" v

-- | Directly use 'Map' as key-values for further converting.
withKeyValues :: T.Text -> (V.Vector (Value, Value) -> Converter a) -> Value -> Converter a
{-# INLINE withKeyValues #-}
withKeyValues _    f (Map kvs) = f kvs
withKeyValues name _ v            = typeMismatch name "Map" v

-- | Take a 'Map' as an 'FM.FlatMap Value Value', on key duplication prefer first one.
withFlatMap :: T.Text -> (FM.FlatMap Value Value -> Converter a) -> Value -> Converter a
{-# INLINE withFlatMap #-}
withFlatMap _    f (Map obj) = f (FM.packVector obj)
withFlatMap name _ v            = typeMismatch name "Map" v

-- | Take a 'Map' as an 'FM.FlatMap Value Value', on key duplication prefer last one.
withFlatMapR :: T.Text -> (FM.FlatMap Value Value -> Converter a) -> Value -> Converter a
{-# INLINE withFlatMapR #-}
withFlatMapR _    f (Map obj) = f (FM.packVectorR obj)
withFlatMapR name _ v            = typeMismatch name "Map" v

-- | Retrieve the value associated with the given key of an 'Map'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
(.:) :: (CBOR a) => FM.FlatMap Value Value -> T.Text -> Converter a
{-# INLINE (.:) #-}
(.:) = convertField fromValue

-- | Retrieve the value associated with the given key of an 'Map'. The
-- result is 'Nothing' if the key is not present or if its value is 'Nil',
-- or fail if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: (CBOR a) => FM.FlatMap Value Value -> T.Text -> Converter (Maybe a)
{-# INLINE (.:?) #-}
(.:?) = convertFieldMaybe fromValue

-- | Retrieve the value associated with the given key of an 'Map'.
-- The result is 'Nothing' if the key is not present or fail if the
-- value cannot be converted to the desired type.
--
-- This differs from '.:?' by attempting to convert 'Nil' the same as any
-- other CBOR value, instead of interpreting it as 'Nothing'.
(.:!) :: (CBOR a) => FM.FlatMap Value Value -> T.Text -> Converter (Maybe a)
{-# INLINE (.:!) #-}
(.:!) = convertFieldMaybe' fromValue

convertField :: (Value -> Converter a)  -- ^ the field converter (value part of a key value pair)
           -> FM.FlatMap Value Value -> T.Text -> Converter a
{-# INLINE convertField #-}
convertField p obj key = case FM.lookup (Str key) obj of
    Just v -> p v <?> Key key
    _      -> fail' (T.concat $ ["key ", key, " not present"])

-- | Variant of '.:?' with explicit converter function.
convertFieldMaybe :: (Value -> Converter a) -> FM.FlatMap Value Value -> T.Text -> Converter (Maybe a)
{-# INLINE convertFieldMaybe #-}
convertFieldMaybe p obj key = case FM.lookup (Str key) obj of
    Just Nil -> pure Nothing
    Just v    -> Just <$> p v <?> Key key
    _         -> pure Nothing

-- | Variant of '.:!' with explicit converter function.
convertFieldMaybe' :: (Value -> Converter a) -> FM.FlatMap Value Value -> T.Text -> Converter (Maybe a)
{-# INLINE convertFieldMaybe' #-}
convertFieldMaybe' p obj key = case FM.lookup (Str key) obj of
    Just v -> Just <$> p v <?> Key key
    _      -> pure Nothing

--------------------------------------------------------------------------------

-- | A newtype for 'B.Builder', whose semigroup's instance is to connect kv builder and sum kv length.
data KVItem = KVItem {-# UNPACK #-} !Int (B.Builder ())

instance Semigroup KVItem where
    {-# INLINE (<>) #-}
    KVItem siza a <> KVItem sizb b = KVItem (siza+sizb) (a >> b)

-- | Connect key and value to a 'KVItem' using 'B.colon', key will be escaped.
(.!) :: CBOR v => T.Text -> v -> KVItem
{-# INLINE (.!) #-}
k .! v = KVItem 1 (CB.str k >> encodeCBOR v)
infixr 8 .!

-- | Write map header and 'KVItem's.
object' :: KVItem -> B.Builder ()
{-# INLINE object' #-}
object' (KVItem siz kvb) = CB.mapHeader siz >> kvb

-- | Connect key and value to a tuple to be used with 'object'.
(.=) :: CBOR v => T.Text -> v -> (Value, Value)
{-# INLINE (.=) #-}
k .= v = (Str k, toValue v)
infixr 8 .=

-- | Alias for @Map . pack@.
object :: [(Value, Value)] -> Value
{-# INLINE object #-}
object = Map . V.pack

--------------------------------------------------------------------------------
-- | Generic encode/decode Settings
--
data Settings = Settings
    { fieldFmt  :: String -> T.Text -- ^ format field labels
    , constrFmt :: String -> T.Text -- ^ format constructor names
    , missingKeyAsNil :: Bool      -- ^ take missing field as 'Nil'?
    }

-- | @Settings T.pack T.pack False@
defaultSettings :: Settings
defaultSettings = Settings T.pack T.pack False

--------------------------------------------------------------------------------
-- GToValue
--------------------------------------------------------------------------------

class GToValue f where
    gToValue :: Settings -> f a -> Value

--------------------------------------------------------------------------------
-- Selectors

type family Field f where
    Field (a :*: b) = Field a
    Field (S1 (MetaSel Nothing u ss ds) f) = Value
    Field (S1 (MetaSel (Just l) u ss ds) f) = (Value, Value)

class GWriteFields f where
    gWriteFields :: Settings -> A.SmallMutableArray s (Field f) -> Int -> f a -> ST s ()

instance (ProductSize a, GWriteFields a, GWriteFields b, Field a ~ Field b) => GWriteFields (a :*: b) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx (a :*: b) = do
        gWriteFields s marr idx a
        gWriteFields s marr (idx + productSize (proxy# :: Proxy# a)) b

instance (GToValue f) => GWriteFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx (M1 x) = A.writeSmallArray marr idx (gToValue s x)

instance (GToValue f, Selector (MetaSel (Just l) u ss ds)) => GWriteFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gWriteFields #-}
    gWriteFields s marr idx m1@(M1 x) = A.writeSmallArray marr idx ((Str $ (fieldFmt s) (selName m1)), gToValue s x)

instance (GToValue f, Selector (MetaSel (Just l) u ss ds)) => GToValue (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gToValue #-}
    gToValue s m1@(M1 x) =
        let k = fieldFmt s $ selName m1
            v = gToValue s x
        in Map (V.singleton (Str k, v))

instance GToValue f => GToValue (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gToValue #-}
    gToValue s (M1 x) = gToValue s x

instance CBOR a => GToValue (K1 i a) where
    {-# INLINE gToValue #-}
    gToValue _ (K1 x) = toValue x

class GMergeFields f where
    gMergeFields :: Proxy# f -> A.SmallMutableArray s (Field f) -> ST s Value

instance GMergeFields a => GMergeFields (a :*: b) where
    {-# INLINE gMergeFields #-}
    gMergeFields _ = gMergeFields (proxy# :: Proxy# a)

instance GMergeFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gMergeFields #-}
    gMergeFields _ marr = do
        arr <- A.unsafeFreezeSmallArray marr
        let l = A.sizeofSmallArray arr
        pure (Array (V.Vector arr 0 l))

instance GMergeFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gMergeFields #-}
    gMergeFields _ marr = do
        arr <- A.unsafeFreezeSmallArray marr
        let l = A.sizeofSmallArray arr
        pure (Map (V.Vector arr 0 l))

--------------------------------------------------------------------------------
-- Constructors

class GConstrToValue f where
    gConstrToValue :: Bool -> Settings -> f a -> Value

instance GConstrToValue V1 where
    {-# INLINE gConstrToValue #-}
    gConstrToValue _ _ _ = error "Z.Data.CBOR.Base: empty data type"

instance (GConstrToValue f, GConstrToValue g) => GConstrToValue (f :+: g) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue _ s (L1 x) = gConstrToValue True s x
    gConstrToValue _ s (R1 x) = gConstrToValue True s x

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrToValue (C1 c U1) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue _ s (M1 _) = Str . constrFmt s $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GToValue (S1 sc f)) => GConstrToValue (C1 c (S1 sc f)) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue False s (M1 x) = gToValue s x
    gConstrToValue True s (M1 x) =
        let !k = constrFmt s $ conName @c undefined
            !v = gToValue s x
        in Map (V.singleton (Str k, v))

-- | Constructor with multiple payloads
instance (ProductSize (a :*: b), GWriteFields (a :*: b), GMergeFields (a :*: b), Constructor c)
    => GConstrToValue (C1 c (a :*: b)) where
    {-# INLINE gConstrToValue #-}
    gConstrToValue False s (M1 x) = runST (do
        marr <- A.newSmallArray (productSize (proxy# :: Proxy# (a :*: b))) undefined
        gWriteFields s marr 0 x
        gMergeFields (proxy# :: Proxy# (a :*: b)) marr)
    gConstrToValue True s (M1 x) =
        let !k = constrFmt s $ conName @c undefined
            !v = runST (do
                    marr <- A.newSmallArray (productSize (proxy# :: Proxy# (a :*: b))) undefined
                    gWriteFields s marr 0 x
                    gMergeFields (proxy# :: Proxy# (a :*: b)) marr)
        in Map (V.singleton (Str k, v))

--------------------------------------------------------------------------------
-- Data types
instance GConstrToValue f => GToValue (D1 c f) where
    {-# INLINE gToValue #-}
    gToValue s (M1 x) = gConstrToValue False s x

--------------------------------------------------------------------------------
-- CBOR
--------------------------------------------------------------------------------

class GEncodeCBOR f where
    gEncodeCBOR :: Settings -> f a -> B.Builder ()

--------------------------------------------------------------------------------
-- Selectors

instance (GEncodeCBOR f, Selector (MetaSel (Just l) u ss ds)) => GEncodeCBOR (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gEncodeCBOR #-}
    gEncodeCBOR s m1@(M1 x) = (CB.str . fieldFmt s $ selName m1) >> gEncodeCBOR s x

instance GEncodeCBOR f => GEncodeCBOR (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gEncodeCBOR #-}
    gEncodeCBOR s (M1 x) = gEncodeCBOR s x

instance (GEncodeCBOR a, GEncodeCBOR b) => GEncodeCBOR (a :*: b) where
    {-# INLINE gEncodeCBOR #-}
    gEncodeCBOR s (a :*: b) = gEncodeCBOR s a >> gEncodeCBOR s b

instance CBOR a => GEncodeCBOR (K1 i a) where
    {-# INLINE gEncodeCBOR #-}
    gEncodeCBOR _ (K1 x) = encodeCBOR x

class GAddProductSize (f :: * -> *) where
    gAddProductSize :: Proxy# f -> Int -> B.Builder ()

instance GAddProductSize a => GAddProductSize (a :*: b) where
    {-# INLINE gAddProductSize #-}
    gAddProductSize _ = gAddProductSize (proxy# :: Proxy# a)

instance GAddProductSize (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gAddProductSize #-}
    gAddProductSize _ = CB.arrayHeader

instance GAddProductSize (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gAddProductSize #-}
    gAddProductSize _ = CB.mapHeader

--------------------------------------------------------------------------------
-- Constructors

class GConstrEncodeCBOR f where
    gConstrEncodeCBOR :: Bool -> Settings -> f a -> B.Builder ()

instance GConstrEncodeCBOR V1 where
    {-# INLINE gConstrEncodeCBOR #-}
    gConstrEncodeCBOR _ _ _ = error "Z.Data.CBOR.Base: empty data type"

instance (GConstrEncodeCBOR f, GConstrEncodeCBOR g) => GConstrEncodeCBOR (f :+: g) where
    {-# INLINE gConstrEncodeCBOR #-}
    gConstrEncodeCBOR _ s (L1 x) = gConstrEncodeCBOR True s x
    gConstrEncodeCBOR _ s (R1 x) = gConstrEncodeCBOR True s x

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrEncodeCBOR (C1 c U1) where
    {-# INLINE gConstrEncodeCBOR #-}
    -- There should be no chars need escaping in constructor name
    gConstrEncodeCBOR _ s (M1 _) = CB.str . constrFmt s $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GEncodeCBOR (S1 (MetaSel Nothing u ss ds) f))
    => GConstrEncodeCBOR (C1 c (S1 (MetaSel Nothing u ss ds) f)) where
    {-# INLINE gConstrEncodeCBOR #-}
    gConstrEncodeCBOR False s (M1 x) = do
        gEncodeCBOR s x
    gConstrEncodeCBOR True s (M1 x) = do
        CB.mapHeader 1
        CB.str (constrFmt s $ conName @c undefined)
        gEncodeCBOR s x

instance (Constructor c, GEncodeCBOR (S1 (MetaSel (Just l) u ss ds) f))
    => GConstrEncodeCBOR (C1 c (S1 (MetaSel (Just l) u ss ds) f)) where
    {-# INLINE gConstrEncodeCBOR #-}
    gConstrEncodeCBOR False s (M1 x) = do
        CB.mapHeader 1
        gEncodeCBOR s x
    gConstrEncodeCBOR True s (M1 x) = do
        CB.mapHeader 1
        CB.str (constrFmt s $ conName @c undefined)
        CB.mapHeader 1
        gEncodeCBOR s x

-- | Constructor with multiple payloads
instance (GEncodeCBOR (a :*: b), GAddProductSize (a :*: b), ProductSize (a :*: b), Constructor c)
    => GConstrEncodeCBOR (C1 c (a :*: b)) where
    {-# INLINE gConstrEncodeCBOR #-}
    gConstrEncodeCBOR False s (M1 x) = do
        gAddProductSize (proxy# :: Proxy# (a :*: b)) (productSize (proxy# :: Proxy# (a :*: b)))
        gEncodeCBOR s x
    gConstrEncodeCBOR True s (M1 x) = do
        CB.mapHeader 1
        CB.str (constrFmt s $ conName @c @_ @_ @_ undefined)
        gAddProductSize (proxy# :: Proxy# (a :*: b)) (productSize (proxy# :: Proxy# (a :*: b)))
        gEncodeCBOR s x

--------------------------------------------------------------------------------
-- Data types
instance GConstrEncodeCBOR f => GEncodeCBOR (D1 c f) where
    {-# INLINE gEncodeCBOR #-}
    gEncodeCBOR s (M1 x) = gConstrEncodeCBOR False s x

--------------------------------------------------------------------------------
-- GFromValue
--------------------------------------------------------------------------------

class GFromValue f where
    gFromValue :: Settings -> Value -> Converter (f a)

--------------------------------------------------------------------------------
-- Selectors

type family LookupTable f where
    LookupTable (a :*: b) = LookupTable a
    LookupTable (S1 (MetaSel Nothing u ss ds) f) = V.Vector Value
    LookupTable (S1 (MetaSel (Just l) u ss ds) f) = FM.FlatMap Value Value

class GFromFields f where
    gFromFields :: Settings -> LookupTable f -> Int -> Converter (f a)

instance (ProductSize a, GFromFields a, GFromFields b, LookupTable a ~ LookupTable b)
    => GFromFields (a :*: b) where
    {-# INLINE gFromFields #-}
    gFromFields s v idx = do
        a <- gFromFields s v idx
        b <- gFromFields s v (idx + productSize (proxy# :: Proxy# a))
        pure (a :*: b)

instance (GFromValue f) => GFromFields (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFromFields #-}
    gFromFields s v idx = do
        v' <- V.unsafeIndexM v idx
        M1 <$> gFromValue s v' <?> Index idx

instance (GFromValue f, Selector (MetaSel (Just l) u ss ds)) => GFromFields (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFromFields #-}
    gFromFields s v _ = do
        case FM.lookup (Str fn) v of
            Just v' -> M1 <$> gFromValue s v' <?> Key fn
            _ | missingKeyAsNil s -> M1 <$> gFromValue s Nil <?> Key fn
              | otherwise -> fail' ("Z.Data.CBOR.Base: missing field " <>  fn)
      where
        fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))

instance GFromValue f => GFromValue (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gFromValue #-}
    gFromValue s x = M1 <$> gFromValue s x

instance (GFromValue f, Selector (MetaSel (Just l) u ss ds)) => GFromValue (S1 (MetaSel (Just l) u ss ds) f) where
    {-# INLINE gFromValue #-}
    gFromValue s (Map v) = do
        case FM.lookup (Str fn) (FM.packVectorR v) of
            Just v' -> M1 <$> gFromValue s v' <?> Key fn
            _ | missingKeyAsNil s -> M1 <$> gFromValue s Nil <?> Key fn
              | otherwise -> fail' ("Z.Data.CBOR.Base: missing field " <>  fn)
      where fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))
    gFromValue s v = typeMismatch ("field " <> fn) "Map" v <?> Key fn
      where fn = (fieldFmt s) (selName (undefined :: S1 (MetaSel (Just l) u ss ds) f a))

instance CBOR a => GFromValue (K1 i a) where
    {-# INLINE gFromValue #-}
    gFromValue _ x = K1 <$> fromValue x

class GBuildLookup f where
    gBuildLookup :: Proxy# f -> Int -> T.Text -> Value -> Converter (LookupTable f)

instance (GBuildLookup a, GBuildLookup b) => GBuildLookup (a :*: b) where
    {-# INLINE gBuildLookup #-}
    gBuildLookup _ siz = gBuildLookup (proxy# :: Proxy# a) siz

instance GBuildLookup (S1 (MetaSel Nothing u ss ds) f) where
    {-# INLINE gBuildLookup #-}
    gBuildLookup _ siz name (Array v)
        -- we have to check size here to use 'unsafeIndexM' later
        | siz' /= siz = fail' . B.unsafeBuildText $ do
            "converting "
            T.text name
            " failed, product size mismatch, expected "
            T.int siz
            ", get"
            T.int siz'
        | otherwise = pure v
      where siz' = V.length v
    gBuildLookup _ _   name x         = typeMismatch name "Array" x

instance GBuildLookup (S1 ((MetaSel (Just l) u ss ds)) f) where
    {-# INLINE gBuildLookup #-}
    -- we don't check size, so that duplicated keys are preserved
    gBuildLookup _ _ _ (Map v) = pure $! FM.packVectorR v
    gBuildLookup _ _ name x    = typeMismatch name "Map" x

--------------------------------------------------------------------------------
-- Constructors

class GConstrFromValue f where
    gConstrFromValue :: Bool    -- ^ Is this a sum type(more than one constructor)?
                     -> Settings -> Value -> Converter (f a)

instance GConstrFromValue V1 where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue _ _ _ = error "Z.Data.CBOR.Base: empty data type"

instance (GConstrFromValue f, GConstrFromValue g) => GConstrFromValue (f :+: g) where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue _ s x = (L1 <$> gConstrFromValue True s x) <|> (R1 <$> gConstrFromValue True s x)

-- | Constructor without payload, convert to String
instance (Constructor c) => GConstrFromValue (C1 c U1) where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue _ s (Str x)
        | cn == x   = pure (M1 U1)
        | otherwise = fail' . T.concat $ ["converting ", cn', "failed, unknown constructor name ", x]
      where cn = constrFmt s $ conName (undefined :: t c U1 a)
            cn' = T.pack $ conName (undefined :: t c U1 a)
    gConstrFromValue _ _ v = typeMismatch cn' "String" v
      where cn' = T.pack $ conName (undefined :: t c U1 a)

-- | Constructor with a single payload
instance (Constructor c, GFromValue (S1 sc f)) => GConstrFromValue (C1 c (S1 sc f)) where
    {-# INLINE gConstrFromValue #-}
    -- | Single constructor
    gConstrFromValue False s x = M1 <$> gFromValue s x
    gConstrFromValue True s x = case x of
        Map v -> case V.indexM v 0 of
            Just (Str k, v')
                | k == cn -> M1 <$> gFromValue s v' <?> Key cn
            _             -> fail' .T.concat $ ["converting ", cn', " failed, constructor not found"]
        _ ->  typeMismatch cn' "Map" x
      where cn = constrFmt s $ conName @c undefined
            cn' = T.pack $ conName @c undefined

-- | Constructor with multiple payloads
instance (ProductSize (a :*: b), GFromFields (a :*: b), GBuildLookup (a :*: b), Constructor c)
    => GConstrFromValue (C1 c (a :*: b)) where
    {-# INLINE gConstrFromValue #-}
    gConstrFromValue False s x = do
        t <- gBuildLookup p (productSize p) cn' x
        M1 <$> gFromFields s t 0
      where cn' = T.pack $ conName @c undefined
            p = proxy# :: Proxy# (a :*: b)
    gConstrFromValue True s x = case x of
        Map v -> case V.indexM v 0 of
            Just (Str k, v')
                | k == cn -> do t <- gBuildLookup p (productSize p) cn' v'
                                M1 <$> gFromFields s t 0
            _             -> fail' .T.concat $ ["converting ", cn', " failed, constructor not found"]
        _ ->  typeMismatch cn' "Map" x
      where cn = constrFmt s $ conName @c undefined
            cn' = T.pack $ conName @c undefined
            p = proxy# :: Proxy# (a :*: b)

--------------------------------------------------------------------------------
-- Data types
instance GConstrFromValue f => GFromValue (D1 c f) where
    {-# INLINE gFromValue #-}
    gFromValue s x = M1 <$> gConstrFromValue False s x

--------------------------------------------------------------------------------
-- Built-in Instances
--------------------------------------------------------------------------------
-- | Use 'Nil' as @Proxy a@
instance CBOR (Proxy a) where
    {-# INLINE fromValue #-}; fromValue = fromNil "Proxy" Proxy;
    {-# INLINE toValue #-}; toValue _ = Nil;
    {-# INLINE encodeCBOR #-}; encodeCBOR _ = CB.nil;

instance CBOR Value   where
    {-# INLINE fromValue #-}; fromValue = pure;
    {-# INLINE toValue #-}; toValue = id;
    {-# INLINE encodeCBOR #-}; encodeCBOR = CB.value;

instance CBOR T.Text   where
    {-# INLINE fromValue #-}; fromValue = withStr "Text" pure;
    {-# INLINE toValue #-}; toValue = Str;
    {-# INLINE encodeCBOR #-}; encodeCBOR = CB.str;

-- | default instance prefer later key
instance (Ord a, CBOR a, CBOR b) => CBOR (FM.FlatMap a b) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Z.Data.Vector.FlatMap.FlatMap" $ \ m ->
        let kvs = V.unpack (FM.sortedKeyValues m)
        in FM.packR <$> (forM kvs $ \ (k, v) -> do
            k' <- fromValue k
            v' <- fromValue v <?> Key (T.toText k)
            return (k', v'))
    {-# INLINE toValue #-}
    toValue = Map . V.map (\ (k, v) -> (toValue k, toValue v)) . FM.sortedKeyValues
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.map encodeCBOR encodeCBOR . FM.sortedKeyValues

instance (Ord a, CBOR a) => CBOR (FS.FlatSet a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Vector.FlatSet.FlatSet" $ \ vs ->
        FS.packRN (V.length vs) <$>
            (zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs))
    {-# INLINE toValue #-}
    toValue = Array . V.map' toValue . FS.sortedValues
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.array encodeCBOR . FS.sortedValues

-- | default instance prefer later key
instance (Eq a, Hashable a, CBOR a, CBOR b) => CBOR (HM.HashMap a b) where
    {-# INLINE fromValue #-}
    fromValue = withKeyValues "Data.HashMap.HashMap" $ \ kvs ->
        HM.fromList <$> (forM (V.unpack kvs) $ \ (k, v) -> do
            !k' <- fromValue k
            !v' <- fromValue v <?> Key (T.toText k)
            return (k', v'))
    {-# INLINE toValue #-}
    toValue = Map . V.pack . map (\ (k,v) -> (toValue k, toValue v)) . HM.toList
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.map' encodeCBOR encodeCBOR . HM.toList

instance (Ord a, CBOR a, CBOR b) => CBOR (M.Map a b) where
    {-# INLINE fromValue #-}
    fromValue = withKeyValues "Data.HashMap.HashMap" $ \ kvs ->
        M.fromList <$> (forM (V.unpack kvs) $ \ (k, v) -> do
            !k' <- fromValue k
            !v' <- fromValue v <?> Key (T.toText k)
            return (k', v'))
    {-# INLINE toValue #-}
    toValue = Map . V.pack . map (\ (k,v) -> (toValue k, toValue v)) . M.toList
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.map' encodeCBOR encodeCBOR . M.toList

instance CBOR a => CBOR (FIM.FlatIntMap a) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Z.Data.Vector.FlatIntMap.FlatIntMap" $ \ m ->
        let kvs = FM.sortedKeyValues m
        in FIM.packVectorR <$> (forM kvs $ \ (k, v) -> do
            case k of
                Int k' -> do
                    v' <- fromValue v <?> Key (T.toText k)
                    return (V.IPair (fromIntegral k') v')
                _ -> fail' ("converting Z.Data.Vector.FlatIntMap.FlatIntMap failed, unexpected key " <> (T.toText k)))
    {-# INLINE toValue #-}
    toValue = Map . V.map' toKV . FIM.sortedKeyValues
      where toKV (V.IPair i x) = let !k = Int (fromIntegral i)
                                     !v = toValue x
                                 in (k, v)
    {-# INLINE encodeCBOR #-}
    encodeCBOR m = do
        let kvs = FIM.sortedKeyValues m
        CB.mapHeader (V.length kvs)
        V.traverseVec_ (\ (V.IPair k v) -> CB.int (fromIntegral k) >> encodeCBOR v) kvs

instance CBOR a => CBOR (IM.IntMap a) where
    {-# INLINE fromValue #-}
    fromValue = withKeyValues "Data.IntMap.IntMap" $ \ kvs ->
        IM.fromList <$> (forM (V.unpack kvs) $ \ (k, v) -> do
            case k of
                Int k' -> do
                    v' <- fromValue v <?> Key (T.toText k)
                    return (fromIntegral k', v')
                _ -> fail' ("converting Data.IntMap.IntMap failed, unexpected key " <> (T.toText k)))
    {-# INLINE toValue #-}
    toValue = Map . V.pack . map toKV . IM.toList
      where toKV (i, x) = let !k = Int (fromIntegral i)
                              !v = toValue x
                          in (k, v)
    {-# INLINE encodeCBOR #-}
    encodeCBOR m = do
        CB.mapHeader (IM.size m)
        mapM_ (\ (k, v) -> CB.int (fromIntegral k) >> encodeCBOR v) (IM.toList m)

instance CBOR FIS.FlatIntSet where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Vector.FlatIntSet.FlatIntSet" $ \ vs ->
        FIS.packRN (V.length vs) <$> zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
    {-# INLINE toValue #-}
    toValue = toValue . FIS.sortedValues
    {-# INLINE encodeCBOR #-}
    encodeCBOR = encodeCBOR . FIS.sortedValues

instance CBOR IS.IntSet where
    {-# INLINE fromValue #-}
    fromValue = withArray "Data.IntSet.IntSet" $ \ vs ->
        IS.fromList <$> zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
    {-# INLINE toValue #-}
    toValue = toValue . IS.toList
    {-# INLINE encodeCBOR #-}
    encodeCBOR = encodeCBOR . IS.toList

instance (Ord a, CBOR a) => CBOR (Set.Set a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Data.Set.Set" $ \ vs ->
        Set.fromList <$> zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
    {-# INLINE toValue #-}
    toValue = toValue . Set.toList
    {-# INLINE encodeCBOR #-}
    encodeCBOR = encodeCBOR . Set.toList

instance CBOR a => CBOR (Seq.Seq a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Data.Seq.Seq" $ \ vs ->
        Seq.fromList <$> zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
    {-# INLINE toValue #-}
    toValue = toValue . Foldable.toList
    {-# INLINE encodeCBOR #-}
    encodeCBOR = encodeCBOR . Foldable.toList

instance CBOR a => CBOR (Tree.Tree a) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Data.Tree" $ \obj -> do
        !n <- obj .: "rootLabel"
        !d <- obj .: "subForest"
        pure (Tree.Node n d)
    {-# INLINE toValue #-}
    toValue x = object [ "rootLabel" .= (Tree.rootLabel x) , "subForest" .= (Tree.subForest x) ]
    {-# INLINE encodeCBOR #-}
    encodeCBOR x = object' ( "rootLabel" .! (Tree.rootLabel x) <> "subForest" .! (Tree.subForest x) )

instance CBOR a => CBOR (A.Array a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Array.Array"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.array encodeCBOR

instance CBOR a => CBOR (A.SmallArray a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Array.SmallArray"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.array encodeCBOR

instance (Prim a, CBOR a) => CBOR (A.PrimArray a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Array.PrimArray"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.array encodeCBOR

instance (A.PrimUnlifted a, CBOR a) => CBOR (A.UnliftedArray a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Array.UnliftedArray"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.array encodeCBOR

instance CBOR A.ByteArray where
    {-# INLINE fromValue #-}
    fromValue = withBin "ByteArray" $ \ (V.PrimVector pa@(A.PrimArray ba#) s l) ->
        if A.sizeofArr pa == l && s == 0
        then pure (A.ByteArray ba#)
        else pure $! A.cloneByteArray (A.ByteArray ba#) s l
    {-# INLINE toValue #-}
    toValue (A.ByteArray ba#) = Bin (V.arrVec (A.PrimArray ba#))
    {-# INLINE encodeCBOR #-}
    encodeCBOR (A.ByteArray ba#) = CB.bin (V.arrVec (A.PrimArray ba#))

instance (Prim a, CBOR a) => CBOR (V.PrimVector a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Vector.PrimVector"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.array encodeCBOR

-- | This is an INCOHERENT instance, write 'Bytes' as Bin.
instance {-# INCOHERENT #-} CBOR V.Bytes where
    {-# INLINE fromValue #-}
    fromValue = withBin "Z.Data.Vector.Bytes" pure
    {-# INLINE toValue #-}
    toValue = Bin
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.bin

-- | Write 'CBytes' as Bin not Str.
instance CBOR CBytes.CBytes where
    {-# INLINE fromValue #-}
    fromValue = withBin "Z.Data.CBytes" (pure . CBytes.fromBytes)
    {-# INLINE toValue #-}
    toValue = Bin . CBytes.toBytes
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.bin . CBytes.toBytes

instance CBOR a => CBOR (V.Vector a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Vector.Vector"
        (V.traverseWithIndex $ \ k v -> fromValue v <?> Index k)
    {-# INLINE toValue #-}
    toValue = Array . V.map toValue
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.array encodeCBOR

instance (Eq a, Hashable a, CBOR a) => CBOR (HS.HashSet a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "Z.Data.Vector.FlatSet.FlatSet" $ \ vs ->
        HS.fromList <$>
            (zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs))
    {-# INLINE toValue #-}
    toValue = toValue . HS.toList
    {-# INLINE encodeCBOR #-}
    encodeCBOR = encodeCBOR . HS.toList

instance CBOR a => CBOR [a] where
    {-# INLINE fromValue #-}
    fromValue = withArray "[a]" $ \ vs ->
        zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
    {-# INLINE toValue #-}
    toValue = Array . V.pack . map toValue
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.array' encodeCBOR

-- | This is an INCOHERENT instance, encode 'String' with 'Str'.
instance {-# INCOHERENT #-} CBOR String where
    {-# INLINE fromValue #-}
    fromValue = withStr "String" (pure . T.unpack)
    {-# INLINE toValue #-}
    toValue = Str . T.pack
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.str . T.pack

instance CBOR a => CBOR (NonEmpty a) where
    {-# INLINE fromValue #-}
    fromValue = withArray "NonEmpty" $ \ vs -> do
        l <- zipWithM (\ k v -> fromValue v <?> Index k) [0..] (V.unpack vs)
        case l of (x:xs) -> pure (x :| xs)
                  _      -> fail' "unexpected empty array"
    {-# INLINE toValue #-}
    toValue = toValue . NonEmpty.toList
    {-# INLINE encodeCBOR #-}
    encodeCBOR = encodeCBOR . NonEmpty.toList

instance CBOR Bool where
    {-# INLINE fromValue #-}; fromValue = withBool "Bool" pure;
    {-# INLINE toValue #-}; toValue = Bool;
    {-# INLINE encodeCBOR #-}; encodeCBOR = CB.bool

instance CBOR Char where
    {-# INLINE fromValue #-}
    fromValue = withStr "Char" $ \ t ->
        if (T.length t == 1)
        then pure (T.head t)
        else fail' (T.concat ["converting Char failed, expected a string of length 1"])
    {-# INLINE toValue #-}
    toValue = Str . T.singleton
    {-# INLINE encodeCBOR #-}
    encodeCBOR = CB.str . T.singleton

instance CBOR Double where
    {-# INLINE fromValue #-}
    fromValue (Float d) = pure $! realToFrac d
    fromValue (Double d) = pure d
    fromValue v = typeMismatch "Double" "Float or Double" v
    {-# INLINE toValue #-}; toValue = Double;
    {-# INLINE encodeCBOR #-}; encodeCBOR = CB.double;

instance CBOR Float  where
    {-# INLINE fromValue #-};
    fromValue (Float d) = pure d
    fromValue (Double d) = pure $! realToFrac d
    fromValue v = typeMismatch "Float" "Float or Double" v
    {-# INLINE toValue #-}; toValue = Float;
    {-# INLINE encodeCBOR #-}; encodeCBOR = CB.float;

#define INT_CBOR(typ) \
    instance CBOR typ where \
        {-# INLINE fromValue #-}; \
            fromValue (Int x) = pure $! fromIntegral x; \
            fromValue v = typeMismatch " typ " "Int" v; \
        {-# INLINE toValue #-}; toValue = Int . fromIntegral; \
        {-# INLINE encodeCBOR #-}; encodeCBOR = CB.int . fromIntegral;
INT_CBOR(Int)
INT_CBOR(Int8)
INT_CBOR(Int16)
INT_CBOR(Int32)
INT_CBOR(Int64)
INT_CBOR(Word)
INT_CBOR(Word8)
INT_CBOR(Word16)
INT_CBOR(Word32)
INT_CBOR(Word64)

instance CBOR Ordering where
    {-# INLINE fromValue #-}
    fromValue = withStr "Ordering" $ \ s ->
        case s of
            "LT" -> pure LT
            "EQ" -> pure EQ
            "GT" -> pure GT
            _ -> fail' . T.concat $ ["converting Ordering failed, unexpected ",
                                        s, " expected \"LT\", \"EQ\", or \"GT\""]
    {-# INLINE toValue #-}
    toValue LT = Str "LT"
    toValue EQ = Str "EQ"
    toValue GT = Str "GT"
    {-# INLINE encodeCBOR #-}
    encodeCBOR LT = CB.str "LT"
    encodeCBOR EQ = CB.str "EQ"
    encodeCBOR GT = CB.str "GT"

instance CBOR () where
    {-# INLINE fromValue #-}
    fromValue = withArray "()" $ \ v ->
        if V.null v
        then pure ()
        else fail' "converting () failed, expected an empty array"
    {-# INLINE toValue #-}
    toValue () = Array V.empty
    {-# INLINE encodeCBOR #-}
    encodeCBOR () = CB.arrayHeader 0

instance CBOR a => CBOR (Maybe a) where
    {-# INLINE fromValue #-}
    fromValue Nil = pure Nothing
    fromValue v    = Just <$> fromValue v
    {-# INLINE toValue #-}
    toValue Nothing  = Nil
    toValue (Just x) = toValue x
    {-# INLINE encodeCBOR #-}
    encodeCBOR Nothing  = CB.nil
    encodeCBOR (Just x) = encodeCBOR x

instance (CBOR a, Integral a) => CBOR (Ratio a) where
    {-# INLINE fromValue #-}
    fromValue = withFlatMapR "Rational" $ \obj -> do
        !n <- obj .: "numerator"
        !d <- obj .: "denominator"
        if d == 0
        then fail' "Ratio denominator was 0"
        else pure (n % d)
    {-# INLINE toValue #-}
    toValue x = object [ "numerator" .= (numerator x) , "denominator" .= (denominator x) ]
    {-# INLINE encodeCBOR #-}
    encodeCBOR x = object' ( "numerator" .! (numerator x) <> "denominator" .! (denominator x) )
