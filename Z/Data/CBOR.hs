{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Z.Data.CBOR
Description : Fast CBOR serialization/deserialization
Copyright   : (c) HanFei, 2021
License     : BSD
Maintainer  : s-labs
Stability   : experimental
Portability : non-portable
This module provides an interface similar to "Z.Data.JSON", to work with CBOR binary format.
  * @Maybe a@ convert to 'Nil' in 'Nothing' case, and @a@ in 'Just' case.
The easiest way to use the library is to define target data type, deriving 'GHC.Generics.Generic' and 'CBOR' instances, e.g.
@
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
import GHC.Generics (Generic)
import qualified Z.Data.CBOR as CBOR
import qualified Z.Data.Text as T
data Person = Person {name :: T.Text, age :: Int}
    deriving (Show, Generic)
    deriving anyclass (CBOR.CBOR)
@
-}

module Z.Data.CBOR
  ( -- * CBOR Class
    CBOR(..), Value(..), defaultSettings, Settings(..), JSON.snakeCase, JSON.trainCase
    -- * Encode & Decode
  , readCBORFile, writeCBORFile
  , decode, decode', decodeChunk, decodeChunks, encode, encodeChunks
  , DecodeError, ParseError
    -- * parse into CBOR Value
  , parseValue, parseValue'
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

import           Data.Char
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum
import qualified Data.Monoid                    as Monoid
import           Data.Proxy                     (Proxy (..))
import           Data.Scientific                (Scientific, toBoundedInteger)
import qualified Data.Semigroup                 as Semigroup
import           Data.Tagged                    (Tagged (..))
import           Data.Time                      (Day, DiffTime, LocalTime, NominalDiffTime,
                                                TimeOfDay, UTCTime, ZonedTime)
import           Data.Time.Calendar             (CalendarDiffDays (..), DayOfWeek (..))
import           Data.Time.LocalTime            (CalendarDiffTime (..))
import           Data.Time.Clock.System         (SystemTime (..), utcToSystemTime, systemToUTCTime)
import           Data.Version                   (Version(versionBranch), makeVersion)
import           Foreign.C.Types
import           System.Exit                    (ExitCode(..))
import qualified Z.Data.Builder                 as B
import           Z.Data.CBOR.Base
import qualified Z.Data.CBOR.Builder     as CB
import qualified Z.Data.JSON                    as JSON
import qualified Z.Data.Parser                  as P
import qualified Z.Data.Text                    as T
import           Z.Data.CBytes            (CBytes)
import           Z.IO
import qualified Z.IO.FileSystem as FS

-- | Decode a 'CBOR' instance from file.
readCBORFile :: (HasCallStack, CBOR a) => CBytes -> IO a
readCBORFile p = unwrap "EPARSE" . decode' =<< FS.readFile p

-- | Encode a 'CBOR' instance to file.
writeCBORFile :: (HasCallStack, CBOR a) => CBytes -> a -> IO ()
writeCBORFile p x = FS.writeFile p (encode x)

--------------------------------------------------------------------------------

instance CBOR ExitCode where
    {-# INLINE fromValue #-}
    fromValue (Str "ExitSuccess") = return ExitSuccess
    fromValue (Int x) = return (ExitFailure (fromIntegral x))
    fromValue _ =  fail' "converting ExitCode failed, expected a string or number"

    {-# INLINE toValue #-}
    toValue ExitSuccess     = Str "ExitSuccess"
    toValue (ExitFailure n) = Int (fromIntegral n)

    {-# INLINE encodeCBOR #-}
    encodeCBOR ExitSuccess     = CB.str "ExitSuccess"
    encodeCBOR (ExitFailure n) = B.int n

-- | Only round trip 'versionBranch' as CBOR array.
instance CBOR Version where
    {-# INLINE fromValue #-}
    fromValue v = makeVersion <$> fromValue v
    {-# INLINE toValue #-}
    toValue = toValue . versionBranch
    {-# INLINE encodeCBOR #-}
    encodeCBOR = encodeCBOR . versionBranch

--------------------------------------------------------------------------------

-- | @YYYY-MM-DD@
instance CBOR Day where
    {-# INLINE fromValue #-}
    fromValue = withStr "Day" $ \ t ->
        case P.parse' (P.day <* P.endOfInput) (T.getUTF8Bytes t) of
            Left err -> fail' $ "could not parse date as Day: " <> T.toText err
            Right r  -> return r
    {-# INLINE toValue #-}
    toValue t = Str (B.unsafeBuildText (B.day t))
    {-# INLINE encodeCBOR #-}
    encodeCBOR t = CB.str (B.unsafeBuildText (B.day t))

-- | @YYYY-MM-DDTHH:MM:SS.SSSZ@
instance CBOR LocalTime where
    {-# INLINE fromValue #-}
    fromValue = withStr "LocalTime" $ \ t ->
        case P.parse' (P.localTime <* P.endOfInput) (T.getUTF8Bytes t) of
            Left err -> fail' $ "could not parse date as LocalTime: " <> T.toText err
            Right r  -> return r
    {-# INLINE toValue #-}
    toValue t = Str (B.unsafeBuildText (B.localTime t))
    {-# INLINE encodeCBOR #-}
    encodeCBOR t = CB.str (B.unsafeBuildText (B.localTime t))

-- | @HH:MM:SS.SSS@
instance CBOR TimeOfDay where
    {-# INLINE fromValue #-}
    fromValue = withStr "TimeOfDay" $ \ t ->
        case P.parse' (P.timeOfDay <* P.endOfInput) (T.getUTF8Bytes t) of
            Left err -> fail' $ "could not parse time as TimeOfDay: " <> T.toText err
            Right r  -> return r
    {-# INLINE toValue #-}
    toValue t = Str (B.unsafeBuildText (B.timeOfDay t))
    {-# INLINE encodeCBOR #-}
    encodeCBOR t = CB.str (B.unsafeBuildText (B.timeOfDay t))

instance CBOR DayOfWeek where
    {-# INLINE fromValue #-}
    fromValue (Str "monday"   ) = pure Monday
    fromValue (Str "tuesday"  ) = pure Tuesday
    fromValue (Str "wednesday") = pure Wednesday
    fromValue (Str "thursday" ) = pure Thursday
    fromValue (Str "friday"   ) = pure Friday
    fromValue (Str "saturday" ) = pure Saturday
    fromValue (Str "sunday"   ) = pure Sunday
    fromValue (Str _   )        = fail' "converting DayOfWeek failed, value should be one of weekdays"
    fromValue v                 = typeMismatch "DayOfWeek" "String" v
    {-# INLINE toValue #-}
    toValue Monday    = Str "monday"
    toValue Tuesday   = Str "tuesday"
    toValue Wednesday = Str "wednesday"
    toValue Thursday  = Str "thursday"
    toValue Friday    = Str "friday"
    toValue Saturday  = Str "saturday"
    toValue Sunday    = Str "sunday"
    {-# INLINE encodeCBOR #-}
    encodeCBOR Monday    = CB.str "monday"
    encodeCBOR Tuesday   = CB.str "tuesday"
    encodeCBOR Wednesday = CB.str "wednesday"
    encodeCBOR Thursday  = CB.str "thursday"
    encodeCBOR Friday    = CB.str "friday"
    encodeCBOR Saturday  = CB.str "saturday"
    encodeCBOR Sunday    = CB.str "sunday"


--------------------------------------------------------------------------------

deriving newtype instance CBOR (f (g a)) => CBOR (Compose f g a)
deriving newtype instance CBOR a => CBOR (Semigroup.Min a)
deriving newtype instance CBOR a => CBOR (Semigroup.Max a)
deriving newtype instance CBOR a => CBOR (Semigroup.First a)
deriving newtype instance CBOR a => CBOR (Semigroup.Last a)
deriving newtype instance CBOR a => CBOR (Semigroup.WrappedMonoid a)
deriving newtype instance CBOR a => CBOR (Semigroup.Dual a)
deriving newtype instance CBOR a => CBOR (Monoid.First a)
deriving newtype instance CBOR a => CBOR (Monoid.Last a)
deriving newtype instance CBOR a => CBOR (Identity a)
deriving newtype instance CBOR a => CBOR (Const a b)
deriving newtype instance CBOR b => CBOR (Tagged a b)

--------------------------------------------------------------------------------

deriving newtype instance CBOR CChar
deriving newtype instance CBOR CSChar
deriving newtype instance CBOR CUChar
deriving newtype instance CBOR CShort
deriving newtype instance CBOR CUShort
deriving newtype instance CBOR CInt
deriving newtype instance CBOR CUInt
deriving newtype instance CBOR CLong
deriving newtype instance CBOR CULong
deriving newtype instance CBOR CPtrdiff
deriving newtype instance CBOR CSize
deriving newtype instance CBOR CWchar
deriving newtype instance CBOR CSigAtomic
deriving newtype instance CBOR CLLong
deriving newtype instance CBOR CULLong
deriving newtype instance CBOR CBool
deriving newtype instance CBOR CIntPtr
deriving newtype instance CBOR CUIntPtr
deriving newtype instance CBOR CIntMax
deriving newtype instance CBOR CUIntMax
deriving newtype instance CBOR CClock
deriving newtype instance CBOR CTime
deriving newtype instance CBOR CUSeconds
deriving newtype instance CBOR CSUSeconds
deriving newtype instance CBOR CFloat
deriving newtype instance CBOR CDouble

--------------------------------------------------------------------------------

deriving anyclass instance (CBOR (f a), CBOR (g a), CBOR a) => CBOR (Sum f g a)
deriving anyclass instance (CBOR a, CBOR b) => CBOR (Either a b)
deriving anyclass instance (CBOR (f a), CBOR (g a)) => CBOR (Product f g a)

deriving anyclass instance (CBOR a, CBOR b) => CBOR (a, b)
deriving anyclass instance (CBOR a, CBOR b, CBOR c) => CBOR (a, b, c)
deriving anyclass instance (CBOR a, CBOR b, CBOR c, CBOR d) => CBOR (a, b, c, d)
deriving anyclass instance (CBOR a, CBOR b, CBOR c, CBOR d, CBOR e) => CBOR (a, b, c, d, e)
deriving anyclass instance (CBOR a, CBOR b, CBOR c, CBOR d, CBOR e, CBOR f) => CBOR (a, b, c, d, e, f)
deriving anyclass instance (CBOR a, CBOR b, CBOR c, CBOR d, CBOR e, CBOR f, CBOR g) => CBOR (a, b, c, d, e, f, g)