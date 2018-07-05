{-# LANGUAGE TemplateHaskell #-}
module Test.Pos.Util.AesonTripping where

import qualified Cardano.Crypto.Wallet as CC
import qualified Crypto.Sign.Ed25519 as Ed25519
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import           Data.Fixed
import qualified Data.HashMap.Strict as HM
import           Data.Time.Units (Millisecond)
import           Hedgehog (Property, property, withTests, (===))
import qualified Hedgehog as H
import           Pos.Aeson.Core.Configuration ()
import           Pos.Binary.Class (Raw (..))
import           Pos.Core.Common (Coeff (..), Coin (..), CoinPortion (..),
                     ScriptVersion, SharedSeed (..), TxFeePolicy (..),
                     TxSizeLinear (..))
import           Pos.Core.Configuration
import           Pos.Core.Delegation (HeavyDlgIndex (..))
import           Pos.Core.Genesis (FakeAvvmOptions (..),
                     GenesisAvvmBalances (..), GenesisDelegation (..),
                     GenesisInitializer (..), GenesisProtocolConstants (..),
                     GenesisSpec (..), TestnetBalanceOptions (..))
import           Pos.Core.ProtocolConstants (VssMaxTTL (..), VssMinTTL (..))
import           Pos.Core.Slotting (EpochIndex (..), FlatSlotId)
import           Pos.Core.Update (BlockVersionData (..), SoftforkRule (..))
import           Pos.Crypto (ProtocolMagic (..))
import           Pos.Crypto.Hashing (abstractHash)
import           Pos.Crypto.Signing (ProxyCert (..), ProxySecretKey (..),
                     PublicKey (..), RedeemPublicKey (..))
import           Serokell.Data.Memory.Units (Byte)
import           Test.Pos.Util.TempTH

import           Universum



--------------------------------------------------------------------------------
-- GensisConfiguration
--------------------------------------------------------------------------------

golden_GenesisConfiguration_GCSpec :: Property
golden_GenesisConfiguration_GCSpec = goldenTestJSON exampleGenesisConfiguration_GCSpec "test/json/GenesisConfiguration_GCSpec"

golden_GenesisConfiguration_GCSrc :: Property
golden_GenesisConfiguration_GCSrc = goldenTestJSON exampleGenesisConfiguration_GCSrc "test/json/GenesisConfiguration_GCSrc"

exampleGenesisConfiguration_GCSrc :: GenesisConfiguration
exampleGenesisConfiguration_GCSrc =
    GCSrc
        { gcsFile = "dRaMwdYsH3QA3dChe"
        , gcsHash = abstractHash (Raw "Test")}

exampleGenesisConfiguration_GCSpec :: GenesisConfiguration
exampleGenesisConfiguration_GCSpec =
    GCSpec $ UnsafeGenesisSpec
        exampleGenesisAvvmBalances
        exampleSharedSeed
        exampleGenesisDelegation
        exampleBlockVersionData
        exampleProtocolConstants
        exampleGenesisInitializer

exampleGenesisAvvmBalances :: GenesisAvvmBalances
exampleGenesisAvvmBalances =
    GenesisAvvmBalances
        (HM.fromList [(RedeemPublicKey (Ed25519.PublicKey "Test")
                     , Coin {getCoin = 36524597913081152})
                     ,(RedeemPublicKey (Ed25519.PublicKey "Test")
                     ,Coin {getCoin = 37343863242999412})
                     ])

exampleSharedSeed :: SharedSeed
exampleSharedSeed = SharedSeed (getBytes 8 32)

exampleGenesisDelegation :: GenesisDelegation
exampleGenesisDelegation = UnsafeGenesisDelegation {unGenesisDelegation = HM.fromList
    [(abstractHash (PublicKey (CC.XPub {CC.xpubPublicKey = "Test"
    , CC.xpubChaincode = CC.ChainCode "Test"}))
    , UnsafeProxySecretKey {pskOmega = HeavyDlgIndex $ EpochIndex 683004810334740698
    , pskIssuerPk = PublicKey (CC.XPub {CC.xpubPublicKey = "Test"
    , CC.xpubChaincode = CC.ChainCode "Test"})
    , pskDelegatePk = PublicKey (CC.XPub {CC.xpubPublicKey = "Test"
    , CC.xpubChaincode = CC.ChainCode "Test"})
    , pskCert = ProxyCert (fromRight (error "Something went wrong") $ sig)})]}
  where
    sig = CC.xsignature "\186\229B*\245@^8\ETX\NAKJJ\217\134\218]\DC4\207\
                        \bMg\SOH\197\199\138y\236sw\DELt\225\&9s\175\131\
                        \u!\DC4\217\241\129f\bY\151\252\129\228\&\
                        \2\202\183\254\233\154']\139\241\&8\173\EOT\225\ETX"

exampleBlockVersionData :: BlockVersionData
exampleBlockVersionData = BlockVersionData
                              (999 :: ScriptVersion)
                              (999 :: Millisecond)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (99 :: FlatSlotId)
                              sfrule
                              (TxFeePolicyTxSizeLinear tslin)
                              (EpochIndex 99)
    where
        tslin = TxSizeLinear c1' c2'
        c1' = Coeff (MkFixed 999)
        c2' = Coeff (MkFixed 77)
        sfrule = (SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99))

exampleProtocolConstants :: GenesisProtocolConstants
exampleProtocolConstants = GenesisProtocolConstants
    { gpcK = 37
    , gpcProtocolMagic = ProtocolMagic {getProtocolMagic = 1783847074}
    , gpcVssMaxTTL = VssMaxTTL {getVssMaxTTL = 1477558317}
    , gpcVssMinTTL = VssMinTTL {getVssMinTTL = 744040476}}

exampleGenesisInitializer :: GenesisInitializer
exampleGenesisInitializer = GenesisInitializer
    {giTestBalance = TestnetBalanceOptions
        {tboPoors = 2448641325904532856
        , tboRichmen = 14071205313513960321
        , tboTotalBalance = 10953275486128625216
        , tboRichmenShare = 4.2098713311249885
        , tboUseHDAddresses = True}
        , giFakeAvvmBalance = FakeAvvmOptions
            {faoCount = 17853231730478779264
            , faoOneBalance = 15087947214890024355}
            , giAvvmBalanceFactor = CoinPortion
                 {getCoinPortion = 366832547637728}
                 , giUseHeavyDlg = False
                 , giSeed = 0}

--------------------------------------------------------------------------------

-- from `Test.Pos.Binary.Helpers.GoldenRoundTrip`
goldenTestJSON :: (A.ToJSON a, HasCallStack) => a -> FilePath -> Property
goldenTestJSON x path = withFrozenCallStack $ do
    withTests 1 . property $ do
        goldenJSON <- liftIO $ BS.readFile path
        (LB.toStrict $ A.encode x) === goldenJSON

-- from `Test.Pos.Crypto.Bi`
getBytes :: Int -> Int -> ByteString
getBytes offset len = BS.take len $ BS.drop offset constantByteString

-- from `Test.Pos.Crypto.Bi`
-- | Changing existing values in this string will break existing golden
-- tests, but it us OK to append more data to the end.
constantByteString :: ByteString
constantByteString
    = "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
    \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
    \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
    \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
    \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
    \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

-- From `Test.Pos.Binary.Helpers`
runTests :: [IO Bool] -> IO ()
runTests tests' = do
    result <- and <$> sequence tests'
    unless result
        exitFailure
--------------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkSequential $$discoverGolden

