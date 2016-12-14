-- File auto generated by purescript-bridge! --
module Pos.Wallet.Web.ClientTypes where

import Data.Types (NominalDiffTime)
import Pos.Types.Types (Coin)
import Prim (Boolean, String)

import Data.Generic (class Generic)


data CCurrency =
    ADA 
  | BTC 
  | ETH 

derive instance genericCCurrency :: Generic CCurrency

data CWalletMeta =
    CWalletMeta {
      cwType :: CWalletType
    , cwCurrency :: CCurrency
    , cwName :: String
    , cwLastUsed :: Boolean
    }

derive instance genericCWalletMeta :: Generic CWalletMeta

data CWalletType =
    CWTPersonal 
  | CWTShared 

derive instance genericCWalletType :: Generic CWalletType

data CWallet =
    CWallet {
      cwAddress :: CAddress
    , cwAmount :: Coin
    , cwMeta :: CWalletMeta
    }

derive instance genericCWallet :: Generic CWallet

data CProfile =
    CProfile {
      cpName :: String
    , cpEmail :: String
    , cpPhoneNumber :: String
    , cpPwHash :: String
    , cpPwCreated :: NominalDiffTime
    , cpLocale :: String
    }

derive instance genericCProfile :: Generic CProfile

data CTType =
    CTIn CTxMeta
  | CTOut CTxMeta
  | CTInOut CTExMeta

derive instance genericCTType :: Generic CTType

data CTxMeta =
    CTxMeta {
      ctmCurrency :: CCurrency
    , ctmTitle :: String
    , ctmDescription :: String
    , ctmDate :: NominalDiffTime
    }

derive instance genericCTxMeta :: Generic CTxMeta

data CTExMeta =
    CTExMeta {
      cexCurrency :: CCurrency
    , cexTitle :: String
    , cexDescription :: String
    , cexDate :: NominalDiffTime
    , cexRate :: String
    , cexLabel :: String
    , cexAddress :: CAddress
    }

derive instance genericCTExMeta :: Generic CTExMeta

data CAddress =
    CAddress CHash

derive instance genericCAddress :: Generic CAddress

data CHash =
    CHash String

derive instance genericCHash :: Generic CHash

data CTxId =
    CTxId CHash

derive instance genericCTxId :: Generic CTxId

data CTx =
    CTx {
      ctId :: CTxId
    , ctAmount :: Coin
    , ctType :: CTType
    }

derive instance genericCTx :: Generic CTx

