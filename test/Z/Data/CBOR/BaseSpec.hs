{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Z.Data.CBOR.BaseSpec where

import qualified Data.List                      as L
import           Data.Word
import           Data.Int
import           Data.Either
import           GHC.Generics
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.Map.Strict                as M
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as Set
import qualified Data.Tree                      as Tree
import qualified Z.Data.Text                    as T
import qualified Z.Data.Vector                  as V
import qualified Z.Data.Builder                 as B
import           Data.Time                      (Day, DiffTime, LocalTime, NominalDiffTime, TimeOfDay, UTCTime, ZonedTime)
import           Data.Time.Calendar             (CalendarDiffDays (..), DayOfWeek (..))
import           Data.Time.LocalTime            (CalendarDiffTime (..))
import           Data.Time.Clock.System         (SystemTime (..), systemToUTCTime, utcToSystemTime)
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Instances
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck
import qualified Z.Data.CBOR                    as CBOR
import           Z.Data.CBOR (CBOR(..), Value(..))


data T a
    = Nullary
    | Unary Int
    | Product T.Text (Maybe Char) a
    | Record { testOne   :: Double
             , testTwo   :: Maybe Bool
             , testThree :: Maybe a
             }

    | RecordII { testFour   :: Double }
    | List [a]
   deriving (Show, Eq, Generic, CBOR)

mid :: CBOR a => a -> a
mid = fromRight (error "decode failed") . CBOR.decode' . CBOR.encode

intMid :: Int64 -> Int64
intMid = mid

ns :: UTCTime -> UTCTime
ns = systemToUTCTime . utcToSystemTime

encode' :: CBOR a => a -> V.Bytes
encode' = CBOR.encode . toValue

spec :: Spec
spec = modifyMaxSuccess (*5) . modifyMaxSize (*5) $ do
    describe "CBOR Base instances" $ do

        it "Nullary constructor are encoded as text" $
            CBOR.encode (Nullary :: T Int64) === B.build (do
                B.word8 0x67
                "Nullary"
            )

        it "Unary constructor are encoded as single field object" $
            CBOR.encode (Unary 256 :: T ()) === B.build (do
                B.word8 0xA1
                B.word8 0x65
                "Unary"
                B.word8 0x19
                B.word8 0x01
                B.word8 0x00
            )

        it "Product are encoded as array" $
            CBOR.encode (Product "ABC" (Just 'x') (256::Int64)) === B.build (do
                B.word8 0xA1
                B.word8 0x67
                "Product"
                B.word8 0x83
                B.word8 0x63
                "ABC"
                B.word8 0x61
                "x"
                B.word8 0x19
                B.word8 0x01
                B.word8 0x00
            )

        it "Record are encoded as key values" $
            CBOR.encode (Record 0.123456 Nothing (Just (256::Int64))) === B.build (do
                B.word8 0xA1
                B.word8 0x66
                "Record"
                B.word8 0xA3
                B.word8 0x67
                "testOne"
                B.word8 0xFB
                B.encodePrimBE @Double 0.123456
                B.word8 0x67
                "testTwo"
                B.word8 0xF6
                B.word8 0x69
                "testThree"
                B.word8 0x19
                B.word8 0x01
                B.word8 0x00
            )

        it "Record are encoded as key values(single field)" $
            CBOR.encode (RecordII 0.123456 :: T ()) === B.build (do
                B.word8 0xA1
                B.word8 0x68
                "RecordII"
                B.word8 0xA1
                B.word8 0x68
                "testFour"
                B.word8 0xFB
                B.encodePrimBE @Double 0.123456
            )

        -- tests from CBOR suit
        it "int"    $ property $ \(a :: Int   ) -> a `shouldBe` mid a
        it "int8"   $ property $ \(a :: Int8  ) -> a `shouldBe` mid a
        it "int16"  $ property $ \(a :: Int16 ) -> a `shouldBe` mid a
        it "int32"  $ property $ \(a :: Int32 ) -> a `shouldBe` mid a
        it "int64"  $ property $ \(a :: Int64 ) -> a `shouldBe` mid a
        it "word"   $ property $ \(a :: Word  ) -> a `shouldBe` mid a
        it "word8"  $ property $ \(a :: Word8 ) -> a `shouldBe` mid a
        it "word16" $ property $ \(a :: Word16) -> a `shouldBe` mid a
        it "word32" $ property $ \(a :: Word32) -> a `shouldBe` mid a
        it "word64" $ property $ \(a :: Word64) -> a `shouldBe` mid a

        it "()"                                  $ property $ \(a :: ())                                   -> a `shouldBe` mid a
        it "bool"                                $ property $ \(a :: Bool)                                 -> a `shouldBe` mid a
        it "float"                               $ property $ \(a :: Float)                                -> a `shouldBe` mid a
        -- it "integer"                             $ property $ \(a :: Integer)                              -> a `shouldBe` mid a
        it "double"                              $ property $ \(a :: Double)                               -> a `shouldBe` mid a
        it "string"                              $ property $ \(a :: String)                               -> a `shouldBe` mid a
        it "bytes"                               $ property $ \(a :: V.Bytes)                              -> a `shouldBe` mid a
        it "primvector"                          $ property $ \(a :: V.PrimVector Int)                     -> a `shouldBe` mid a
        it "vector"                              $ property $ \(a :: V.Vector [Int64])                     -> a `shouldBe` mid a
        it "maybe int"                           $ property $ \(a :: (Maybe Int))                          -> a `shouldBe` mid a
        it "[int]"                               $ property $ \(a :: [Int])                                -> a `shouldBe` mid a
        it "[string]"                            $ property $ \(a :: [String])                             -> a `shouldBe` mid a
        it "(int, int)"                          $ property $ \(a :: (Int, Int))                           -> a `shouldBe` mid a
        it "(int, int, int)"                     $ property $ \(a :: (Int, Int, Int))                      -> a `shouldBe` mid a
        it "(int, int, int, int)"                $ property $ \(a :: (Int, Int, Int, Int))                 -> a `shouldBe` mid a
        it "(int, int, int, int, int)"           $ property $ \(a :: (Int, Int, Int, Int, Int))            -> a `shouldBe` mid a
        it "(int, int, int, int, int, int)"      $ property $ \(a :: (Int, Int, Int, Int, Int, Int))       -> a `shouldBe` mid a
        it "(int, int, int, int, int, int, int)" $ property $ \(a :: (Int, Int, Int, Int, Int, Int, Int))  -> a `shouldBe` mid a
        it "(int, double)"                       $ property $ \(a :: (Int, Double))                        -> a `shouldBe` mid a
        it "[(int, double)]"                     $ property $ \(a :: [(Int, Double)])                      -> a `shouldBe` mid a
        it "[(string, string)]"                  $ property $ \(a :: [(String, String)])                   -> a `shouldBe` mid a
        it "HashMap Text Int"                    $ property $ \(a :: HM.HashMap T.Text Int)                -> a `shouldBe` mid a
        it "HashSet Text"                        $ property $ \(a :: HS.HashSet T.Text)                    -> a `shouldBe` mid a
        it "Map Text Int"                        $ property $ \(a :: M.Map T.Text Int)                     -> a `shouldBe` mid a
        it "IntMap Int"                          $ property $ \(a :: IM.IntMap Int)                        -> a `shouldBe` mid a
        it "Set Int"                             $ property $ \(a :: Set.Set Int)                          -> a `shouldBe` mid a
        it "IntSet"                              $ property $ \(a :: IS.IntSet)                            -> a `shouldBe` mid a
        it "Seq Int"                             $ property $ \(a :: Seq.Seq Int)                          -> a `shouldBe` mid a
        it "Tree Int"                            $ property $ \(a :: Tree.Tree Int)                        -> a `shouldBe` mid a
        it "maybe int"                           $ property $ \(a :: Maybe Int)                            -> a `shouldBe` mid a
        it "maybe nil"                           $ property $ \(a :: Maybe ())                             -> a `shouldBe` mid a
        it "maybe bool"                          $ property $ \(a :: Maybe Bool)                           -> a `shouldBe` mid a
        it "maybe double"                        $ property $ \(a :: Maybe Double)                         -> a `shouldBe` mid a
        it "maybe string"                        $ property $ \(a :: Maybe String)                         -> a `shouldBe` mid a
        it "maybe bytes"                         $ property $ \ (a :: Maybe V.Bytes)                       -> a `shouldBe` mid a
        it "maybe [int]"                         $ property $ \(a :: Maybe [Int])                          -> a `shouldBe` mid a
        it "maybe [string]"                      $ property $ \(a :: Maybe [String])                       -> a `shouldBe` mid a
        it "maybe (int, int)"                    $ property $ \(a :: Maybe (Int, Int))                     -> a `shouldBe` mid a
        it "maybe (int, int, int)"               $ property $ \(a :: Maybe (Int, Int, Int))                -> a `shouldBe` mid a
        it "maybe (int, int, int, int)"          $ property $ \(a :: Maybe (Int, Int, Int, Int))           -> a `shouldBe` mid a
        it "maybe (int, int, int, int, int)"     $ property $ \(a :: Maybe (Int, Int, Int, Int, Int))      -> a `shouldBe` mid a
        it "maybe [(int, double)]"               $ property $ \(a :: Maybe [(Int, Double)])                -> a `shouldBe` mid a
        it "maybe [(string, string)]"            $ property $ \(a :: Maybe [(String, String)])             -> a `shouldBe` mid a
        it "either int float"                    $ property $ \(a :: Either Int Float)                     -> a `shouldBe` mid a
        it "Day"                                 $ property $ \(a :: Day)                                  -> a `shouldBe` mid a
        -- it "DiffTime"                            $ property $ \(a :: DiffTime)                             -> a `shouldBe` mid a
        it "LocalTime"                           $ property $ \(a :: LocalTime)                            -> a `shouldBe` mid a
        -- it "NominalDiffTime"                     $ property $ \(a :: NominalDiffTime)                      -> a `shouldBe` mid a
        it "TimeOfDay"                           $ property $ \(a :: TimeOfDay)                            -> a `shouldBe` mid a
        -- it "UTCTime"                             $ property $ \(a :: UTCTime)                              -> ns a `shouldBe` mid (ns a)
        -- it "SystemTime"                          $ property $ \(a :: SystemTime)                           -> a `shouldBe` mid a
        -- it "CalendarDiffDays"                    $ property $ \(a :: CalendarDiffDays)                     -> a `shouldBe` mid a
        it "DayOfWeek"                           $ property $ \(a :: DayOfWeek)                            -> a `shouldBe` mid a
        -- it "CalendarDiffTime"                    $ property $ \(a :: CalendarDiffTime)                     -> a `shouldBe` mid a
        it "arbitrary message"                   $ property $ \(a :: Value)                                -> a `shouldBe` mid a

    describe "CBOR Base instances encodeCBOR == encodeCBOR . toValue" $ do

        it "Nullary constructor are encoded as text" $
            CBOR.encode (Nullary :: T Int64) ===
                encode' (Nullary :: T Int64)

        it "Unary constructor are encoded as single field object" $
            CBOR.encode (Unary 123456 :: T Int64) ===
                encode' (Unary 123456 :: T Int64)

        it "Product are encoded as array" $
            CBOR.encode (Product "ABC" (Just 'x') (123456::Int64)) ===
                encode' (Product "ABC" (Just 'x') (123456::Int64))

        it "Record are encoded as key values" $
            CBOR.encode (Record 0.123456 Nothing (Just (123456::Int64))) ===
                encode' (Record 0.123456 Nothing (Just (123456::Int64)))

        it "Record are encoded as key values(single field)" $
            CBOR.encode (RecordII 0.123456 :: T Int64) ===
                encode' (RecordII 0.123456 :: T Int64)

        it "List are encode as array" $
            CBOR.encode (List [Nullary
                , Unary 123456
                , (Product "ABC" (Just 'x') (123456::Int64))
                , (Record 0.123456 Nothing (Just (123456::Int64)))]) ===
                encode' (List [Nullary
                    , Unary 123456
                    , (Product "ABC" (Just 'x') (123456::Int64))
                    , (Record 0.123456 Nothing (Just (123456::Int64)))])

        it "control characters are escaped" $
            CBOR.encode (T.pack $ map toEnum [0..0x1F]) ===
                encode' (T.pack $ map toEnum [0..0x1F])

        -- tests from CBOR suit
        it "int"    $ property $ \(a :: Int   ) -> encode' a === CBOR.encode a
        it "int8"   $ property $ \(a :: Int8  ) -> encode' a === CBOR.encode a
        it "int16"  $ property $ \(a :: Int16 ) -> encode' a === CBOR.encode a
        it "int32"  $ property $ \(a :: Int32 ) -> encode' a === CBOR.encode a
        it "int64"  $ property $ \(a :: Int64 ) -> encode' a === CBOR.encode a
        it "word"   $ property $ \(a :: Word  ) -> encode' a === CBOR.encode a
        it "word8"  $ property $ \(a :: Word8 ) -> encode' a === CBOR.encode a
        it "word16" $ property $ \(a :: Word16) -> encode' a === CBOR.encode a
        it "word32" $ property $ \(a :: Word32) -> encode' a === CBOR.encode a
        it "word64" $ property $ \(a :: Word64) -> encode' a === CBOR.encode a

        it "()"                                  $ property $ \(a :: ())                                   -> encode' a === CBOR.encode a
        it "bool"                                $ property $ \(a :: Bool)                                 -> encode' a === CBOR.encode a
        it "float"                               $ property $ \(a :: Float)                                -> encode' a === CBOR.encode a
        it "double"                              $ property $ \(a :: Double)                               -> encode' a === CBOR.encode a
        it "integer"                             $ property $ \(a :: Int64)                                -> encode' a === CBOR.encode a
        it "string"                              $ property $ \(a :: String)                               -> encode' a === CBOR.encode a
        it "bytes"                               $ property $ \(a :: V.Bytes)                              -> encode' a === CBOR.encode a
        it "primvector"                          $ property $ \(a :: V.PrimVector Int)                     -> encode' a === CBOR.encode a
        it "vector"                              $ property $ \(a :: V.Vector [Int64])                     -> encode' a === CBOR.encode a
        it "maybe int"                           $ property $ \(a :: (Maybe Int))                          -> encode' a === CBOR.encode a
        it "[int]"                               $ property $ \(a :: [Int])                                -> encode' a === CBOR.encode a
        it "[string]"                            $ property $ \(a :: [String])                             -> encode' a === CBOR.encode a
        it "(int, int)"                          $ property $ \(a :: (Int, Int))                           -> encode' a === CBOR.encode a
        it "(int, int, int)"                     $ property $ \(a :: (Int, Int, Int))                      -> encode' a === CBOR.encode a
        it "(int, int, int, int)"                $ property $ \(a :: (Int, Int, Int, Int))                 -> encode' a === CBOR.encode a
        it "(int, int, int, int, int)"           $ property $ \(a :: (Int, Int, Int, Int, Int))            -> encode' a === CBOR.encode a
        it "(int, int, int, int, int, int)"      $ property $ \(a :: (Int, Int, Int, Int, Int, Int))       -> encode' a === CBOR.encode a
        it "(int, int, int, int, int, int, int)" $ property $ \(a :: (Int, Int, Int, Int, Int, Int, Int))  -> encode' a === CBOR.encode a
        it "(int, double)"                       $ property $ \(a :: (Int, Double))                        -> encode' a === CBOR.encode a
        it "[(int, double)]"                     $ property $ \(a :: [(Int, Double)])                      -> encode' a === CBOR.encode a
        it "[(string, string)]"                  $ property $ \(a :: [(String, String)])                   -> encode' a === CBOR.encode a
        it "HashMap Text Int"                    $ property $ \(a :: HM.HashMap T.Text Int)                -> encode' a === CBOR.encode a
        it "HashSet Text"                        $ property $ \(a :: HS.HashSet T.Text)                    -> encode' a === CBOR.encode a
        it "Map Text Int"                        $ property $ \(a :: M.Map T.Text Int)                     -> encode' a === CBOR.encode a
        it "IntMap Int"                          $ property $ \(a :: IM.IntMap Int)                        -> encode' a === CBOR.encode a
        it "Set Int"                             $ property $ \(a :: Set.Set Int)                          -> encode' a === CBOR.encode a
        it "IntSet"                              $ property $ \(a :: IS.IntSet)                            -> encode' a === CBOR.encode a
        it "Seq Int"                             $ property $ \(a :: Seq.Seq Int)                          -> encode' a === CBOR.encode a
        it "Tree Int"                            $ property $ \(a :: Tree.Tree Int)                        -> encode' a === CBOR.encode a
        it "maybe int"                           $ property $ \(a :: Maybe Int)                            -> encode' a === CBOR.encode a
        it "maybe nil"                           $ property $ \(a :: Maybe ())                             -> encode' a === CBOR.encode a
        it "maybe bool"                          $ property $ \(a :: Maybe Bool)                           -> encode' a === CBOR.encode a
        it "maybe double"                        $ property $ \(a :: Maybe Double)                         -> encode' a === CBOR.encode a
        it "maybe string"                        $ property $ \(a :: Maybe String)                         -> encode' a === CBOR.encode a
        it "maybe bytes"                         $ property $ \ (a :: Maybe V.Bytes)                       -> encode' a === CBOR.encode a
        it "maybe [int]"                         $ property $ \(a :: Maybe [Int])                          -> encode' a === CBOR.encode a
        it "maybe [string]"                      $ property $ \(a :: Maybe [String])                       -> encode' a === CBOR.encode a
        it "maybe (int, int)"                    $ property $ \(a :: Maybe (Int, Int))                     -> encode' a === CBOR.encode a
        it "maybe (int, int, int)"               $ property $ \(a :: Maybe (Int, Int, Int))                -> encode' a === CBOR.encode a
        it "maybe (int, int, int, int)"          $ property $ \(a :: Maybe (Int, Int, Int, Int))           -> encode' a === CBOR.encode a
        it "maybe (int, int, int, int, int)"     $ property $ \(a :: Maybe (Int, Int, Int, Int, Int))      -> encode' a === CBOR.encode a
        it "maybe [(int, double)]"               $ property $ \(a :: Maybe [(Int, Double)])                -> encode' a === CBOR.encode a
        it "maybe [(string, string)]"            $ property $ \(a :: Maybe [(String, String)])             -> encode' a === CBOR.encode a
        it "either int float"                    $ property $ \(a :: Either Int Float)                     -> encode' a === CBOR.encode a
        it "Day"                                 $ property $ \(a :: Day)                                  -> encode' a === CBOR.encode a
        -- it "DiffTime"                            $ property $ \(a :: DiffTime)                             -> encode' a === CBOR.encode a
        it "LocalTime"                           $ property $ \(a :: LocalTime)                            -> encode' a === CBOR.encode a
        -- it "NominalDiffTime"                     $ property $ \(a :: NominalDiffTime)                      -> encode' a === CBOR.encode a
        it "TimeOfDay"                           $ property $ \(a :: TimeOfDay)                            -> encode' a === CBOR.encode a
        -- it "UTCTime"                             $ property $ \(a :: UTCTime)                              -> encode' a === CBOR.encode a
        -- it "SystemTime"                          $ property $ \(a :: SystemTime)                           -> encode' a === CBOR.encode a
        -- it "CalendarDiffDays"                    $ property $ \(a :: CalendarDiffDays)                     -> encode' a === CBOR.encode a
        it "DayOfWeek"                           $ property $ \(a :: DayOfWeek)                            -> encode' a === CBOR.encode a
        -- it "CalendarDiffTime"                    $ property $ \(a :: CalendarDiffTime)                     -> encode' a === CBOR.encode a