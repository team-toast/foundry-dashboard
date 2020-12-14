module Types exposing (..)

import Array exposing (Array)
import Browser
import Browser.Navigation
import Common.Msg exposing (..)
import Common.Types exposing (..)
import Dict exposing (Dict)
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet exposing (WalletSentry)
import Eth.Types exposing (Address, Hex, Tx, TxHash, TxReceipt)
import Farm.Types as Farm
import Helpers.Element as EH
import Home.Types as Home
import Routing exposing (Route)
import Sentiment.Types as Sentiment
import Stats.Types as Stats
import Time
import TokenValue exposing (TokenValue)
import Url exposing (Url)
import UserNotice exposing (UserNotice)
import UserTx exposing (TxInfo)
import Wallet exposing (Wallet)


type alias Flags =
    { basePath : String
    , networkId : Int
    , width : Int
    , height : Int
    , nowInMillis : Int
    , cookieConsent : Bool
    }


type alias Model =
    { navKey : Browser.Navigation.Key
    , basePath : String
    , route : Route
    , wallet : Wallet
    , now : Time.Posix
    , dProfile : EH.DisplayProfile
    , txSentry : TxSentry Msg
    , eventSentry : EventSentry Msg
    , submodel : Submodel
    , showAddressId : Maybe PhaceIconId
    , userNotices : List UserNotice
    , trackedTxs : UserTx.Tracker Msg
    , trackedTxsExpanded : Bool
    , nonRepeatingGTagsSent : List String
    , cookieConsentGranted : Bool
    }


type Submodel
    = BlankInitialSubmodel
    | Home Home.Model
    | Sentiment Sentiment.Model
    | Stats Stats.Model
    | Farm Farm.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Tick Time.Posix
    | EveryFewSeconds
    | Resize Int Int
    | WalletStatus (Result String WalletSentry)
    | TxSentryMsg TxSentry.Msg
    | EventSentryMsg EventSentry.Msg
    | DismissNotice Int
    | ClickHappened
    | ShowExpandedTrackedTxs Bool
    | TxSigned Int (Result String TxHash)
    -- | TxBroadcast Int (Result String Tx)
    | TxMined Int (Result String TxReceipt)
    | HomeMsg Home.Msg
    | SentimentMsg Sentiment.Msg
    | StatsMsg Stats.Msg
    | FarmMsg Farm.Msg
      -- | BalanceFetched Address (Result Http.Error TokenValue)
    | CookieConsentGranted
    | MsgUp MsgUp
    | NoOp
