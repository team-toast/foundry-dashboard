module Types exposing (..)

import AddressDict exposing (AddressDict)
import Array exposing (Array)
import Browser
import Browser.Navigation
import Dict exposing (Dict)
import ElementHelpers as EH
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet exposing (WalletSentry)
import Eth.Types exposing (Address, Hex, Tx, TxHash, TxReceipt)
import Graphql.Http
import Http
import Json.Decode
import Routing exposing (Route)
import Time
import TokenValue exposing (TokenValue)
import Url exposing (Url)
import UserNotice exposing (UserNotice)
import UserTx


type alias Flags =
    { basePath : String
    , networkId : Int
    , width : Int
    , height : Int
    , nowInMillis : Int
    , cookieConsent : Bool
    , ethProviderUrl : String
    , xDaiProviderUrl : String
    , bscProviderUrl : String
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
    , showAddressId : Maybe PhaceIconId
    , userNotices : List UserNotice
    , trackedTxs : UserTx.Tracker Msg
    , trackedTxsExpanded : Bool
    , nonRepeatingGTagsSent : List String
    , cookieConsentGranted : Bool
    , currentTime : Int
    , currentBucketId : Maybe Int
    , currentBucketTotalEntered : Maybe TokenValue
    , currentEthPriceUsd : Maybe Float
    , currentDaiPriceEth : Maybe Float
    , currentFryPriceEth : Maybe Float
    , circSupply : Maybe Float
    , marketCap : Maybe Float
    , fullyDiluted : Maybe Float
    , permaFrostedTokens : Maybe TokenValue
    , teamTokenBalances : Array (Maybe TokenValue)
    , balancerFryBalance : Maybe TokenValue
    , permaFrostTotalSupply : Maybe TokenValue
    , permaFrostBalanceLocked : Maybe TokenValue
    , treasuryBalance : Maybe TokenValue
    , userDerivedEthInfo : Maybe UserDerivedEthInfo
    , jurisdictionCheckStatus : JurisdictionCheckStatus
    , depositAmount : String
    , withDrawalAmount : String
    , polls : Maybe (List Poll)
    , maybeValidResponses : Dict Int ( Bool, SignedResponse ) -- bool represents whether the validation test has been ATTEMPTED, not whether it PASSED
    , validatedResponses : ValidatedResponseTracker
    , fryBalances : AddressDict (Maybe TokenValue)
    , mouseoverState : MouseoverState
    , userStakingInfo : Maybe UserStakingInfo
    , apy : Maybe Float
    , depositWithdrawUXModel : DepositOrWithdrawUXModel
    , farmingIsActive : Bool
    , networkId : Maybe Int
    }


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
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
    | TxMined Int (Result String TxReceipt)
    | CookieConsentGranted
    | BucketValueEnteredFetched (Maybe Int) (Result Http.Error TokenValue)
    | FetchedEthPrice (Result (Graphql.Http.Error (Maybe Value)) (Maybe Value))
    | FetchedDaiPrice (Result (Graphql.Http.Error (Maybe Value)) (Maybe Value))
    | FetchedFryPrice (Result (Graphql.Http.Error (Maybe Value)) (Maybe Value))
    | FetchedTeamTokens Int (Result Http.Error TokenValue)
    | FetchedPermaFrostBalanceLocked (Result Http.Error TokenValue)
    | FetchedPermaFrostTotalSupply (Result Http.Error TokenValue)
    | FetchedBalancerFryBalance (Result Http.Error TokenValue)
    | FetchedTreasuryBalance (Result Http.Error TokenValue)
    | DepositAmountChanged String
    | WithdrawalAmountChanged String
    | DepositClicked TokenValue
    | WithdrawClicked TokenValue
    | UserEthBalanceFetched (Result Http.Error TokenValue)
    | UserDerivedEthBalanceFetched (Result Http.Error TokenValue)
    | DerivedEthRedeemableFetched (Result Http.Error ( TokenValue, TokenValue, TokenValue ))
    | VerifyJurisdictionClicked
    | LocationCheckResult (Result Json.Decode.Error (Result String LocationInfo))
    | ApproveTokenSpend
    | DepositSigned (Result String TxHash)
    | WithdrawSigned (Result String TxHash)
    | FetchUserEthBalance
    | FetchUserDerivedEthBalance
    | DerivedEthIssuanceDetailFetched (Result Http.Error ( TokenValue, TokenValue, TokenValue ))
    | GotoRoute Route
    | ConnectToWeb3
    | ShowOrHideAddress PhaceIconId
    | AddUserNotice UserNotice
    | RefreshAll
    | PollsFetched (Result Http.Error (List Poll))
    | OptionClicked (Maybe UserInfo) Poll (Maybe Int)
    | Web3SignResultValue Json.Decode.Value
    | Web3ValidateSigResultValue Json.Decode.Value
    | ResponseSent Int (Result Http.Error ())
    | SignedResponsesFetched (Result Http.Error (Dict Int SignedResponse))
    | FryBalancesFetched (Result Http.Error (AddressDict TokenValue))
    | SetMouseoverState MouseoverState
    | UpdateNow Time.Posix
    | AmountInputChanged String
    | UXBack
    | StartDeposit TokenValue
    | DoUnlock
    | DoDeposit TokenValue
    | DoClaimRewards
    | DoExit
    | StartWithdraw TokenValue
    | DoWithdraw TokenValue
    | DepositOrWithdrawSigned DepositOrWithdraw TokenValue (Result String TxHash)
    | StakingInfoFetched (Result Http.Error ( UserStakingInfo, Float ))
    | ApyFetched (Result Http.Error Float)
    | RefetchStakingInfoOrApy
    | Navigate Route


type alias Value =
    { ethPrice : Float
    }


type alias UserInfo =
    { network : Eth.Net.NetworkId
    , address : Address
    }


type PhaceIconId
    = UserPhace


type alias UserStakingInfo =
    { unstaked : TokenValue
    , allowance : TokenValue
    , staked : TokenValue
    , claimableRewards : TokenValue
    , rewardRate : TokenValue
    , timestamp : Time.Posix
    }


type alias UserDerivedEthInfo =
    { ethBalance : TokenValue
    , dEthBalance : TokenValue
    , totalCollateralRedeemed : TokenValue
    , redeemFee : TokenValue
    , collateralReturned : TokenValue
    , dEthAllowance : TokenValue
    , actualCollateralAdded : TokenValue
    , depositFee : TokenValue
    , tokensIssued : TokenValue
    }


type alias DepositOrWithdrawUXModel =
    Maybe ( DepositOrWithdraw, AmountUXModel )


type alias AmountUXModel =
    { amountInput : String
    }


type DepositOrWithdraw
    = Deposit
    | Withdraw


type Jurisdiction
    = ForbiddenJurisdictions
    | JurisdictionsWeArentIntimidatedIntoExcluding


type JurisdictionCheckStatus
    = WaitingForClick
    | Checking
    | Checked Jurisdiction
    | Error String


type alias LocationInfo =
    { ipCode : String
    , geoCode : String
    }


type MouseoverState
    = None
    | VoterBlock VoterBlockMouseoverInfo


type alias VoterBlockMouseoverInfo =
    { pollId : Int
    , pollOptionId : Int
    , blockId : Int
    }


type alias ValidatedResponseTracker =
    Dict Int (AddressDict ValidatedResponse)


type alias Poll =
    { id : Int
    , title : String
    , question : String
    , options : List PollOption
    }


type alias PollOption =
    { id : Int
    , pollId : Int
    , name : String
    }


type alias SignedResponse =
    { address : Address
    , pollId : Int
    , maybePollOptionId : Maybe Int
    , sig : String
    }


type alias ResponseToValidate =
    { id : Int
    , data : String
    , sig : String
    , address : Address
    }


type alias LoggedSignedResponse =
    ( Int, SignedResponse )


type alias ValidatedResponse =
    { id : Int
    , maybePollOptionId : Maybe Int
    }


type SigValidationResult
    = Valid
    | Invalid


type Wallet
    = NoneDetected
    | OnlyNetwork Eth.Net.NetworkId
    | Active UserInfo


type alias VoteBarBlock =
    { x : Int
    , width : Int
    , colorRgb : ( Float, Float, Float )
    , address : Address
    , amount : TokenValue
    }


type InputValidationResult
    = InputValid
    | InputLessThan
    | InputGreaterThan
    | InputUndefined


type Chain
    = XDai
    | Eth
    | BSC


type alias ChainConfig =
    { chain : Chain
    , contract : Address
    , startScanBlock : Int
    , providerUrl : String
    }


type alias Config =
    { xDai : ChainConfig
    , ethereum : ChainConfig
    , bsc : ChainConfig
    }


type WalletConnectErr
    = WalletCancel
    | WalletInProgress
    | WalletError String
    | NetworkNotSupported
