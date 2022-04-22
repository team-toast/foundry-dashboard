module Types exposing (..)

import AddressDict exposing (AddressDict)
import Array exposing (Array)
import BigInt exposing (BigInt)
import Browser
import Browser.Navigation
import Contracts.Generated.DEth as DethGenerated
import Dict exposing (Dict)
import ElementHelpers as EH
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet exposing (WalletSentry)
import Eth.Types exposing (Address, HttpProvider, TxHash, TxReceipt)
import GTag exposing (..)
import Graphql.Http
import Http
import Json.Decode exposing (Value)
import Routing exposing (Route)
import Time
import TokenValue exposing (TokenValue)
import Url exposing (Url)
import UserNotice exposing (UserNotice)
import UserTx


type alias Flags =
    { basePath : String
    , width : Int
    , height : Int
    , nowInMillis : Int
    , cookieConsent : Bool
    , chains : Value

    -- , ethProviderUrl : String
    -- , xDaiProviderUrl : String
    -- , bscProviderUrl : String
    , hasWallet : Bool
    }


type alias TokenBalanceDict =
    AddressDict (AddressDict (Maybe TokenValue))


type alias Model =
    { navKey : Browser.Navigation.Key
    , basePath : String
    , route : Route
    , wallet : Wallet
    , now : Time.Posix
    , dProfile : EH.DisplayProfile
    , sentries :
        { xDai : EventSentry Msg
        , ethereum : EventSentry Msg
        , bsc : EventSentry Msg
        }
    , txSentry : TxSentry Msg
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
    , dethProfit : Maybe TokenValue
    , dethTVL : Maybe TokenValue
    , permaFrostTotalSupply : Maybe TokenValue
    , permaFrostBalanceLocked : Maybe TokenValue
    , composedTreasuryBalance : ComposedTreasuryBalance
    , userDerivedEthInfo : Maybe UserDerivedEthInfo
    , dEthUserInfo : Maybe DEthUserInfo
    , dEthDepositInfo : Maybe DEthDepositInfo
    , dEthWithdrawInfo : Maybe DEthWithdrawInfo
    , jurisdictionCheckStatus : JurisdictionCheckStatus
    , depositAmountInput : String
    , withdrawalAmountInput : String
    , polls : Maybe (List Poll)
    , possiblyValidResponses : Dict Int ( Bool, SignedResponse ) -- bool represents whether the validation test has been ATTEMPTED, not whether it PASSED
    , validatedResponses : ValidatedResponseTracker
    , fryBalances : TokenBalanceDict
    , unifiedBalances : AddressDict (Maybe TokenValue)
    , mouseoverState : MouseoverState
    , userStakingInfo : Maybe UserStakingInfo
    , oldUserStakingBalances : List ( Address, Maybe TokenValue )
    , apy : Maybe Float
    , depositWithdrawUXModel : DepositOrWithdrawUXModel
    , config : Config
    , chainSwitchInProgress : Bool
    , gtagHistory : GTagHistory
    , farmingPeriodEnds : Int
    , initiatedOldFarmExit : Bool
    , dethGlobalSupply : Maybe TokenValue
    , dethUniqueMints : AddressDict TokenValue
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
    | EventSentryMsg Chain EventSentry.Msg
    | DismissNotice Int
    | ClickHappened
    | ShowExpandedTrackedTxs Bool
    | TxSigned Int (Result String TxHash)
    | TxMined Int (Result String TxReceipt)
    | CookieConsentGranted
    | BucketValueEnteredFetched (Maybe Int) (Result Http.Error TokenValue)
    | FetchedEthPrice (Result (Graphql.Http.Error (Maybe PriceValue)) (Maybe PriceValue))
    | FetchedDaiPrice (Result (Graphql.Http.Error (Maybe PriceValue)) (Maybe PriceValue))
    | FetchedFryPrice (Result (Graphql.Http.Error (Maybe PriceValue)) (Maybe PriceValue))
    | FetchedTeamTokens Int (Result Http.Error TokenValue)
    | FetchedPermaFrostBalanceLocked (Result Http.Error TokenValue)
    | FetchedPermaFrostTotalSupply (Result Http.Error TokenValue)
    | FetchedBalancerFryBalance (Result Http.Error TokenValue)
    | FetchedTreasuryBalance Int (Result Http.Error TokenValue)
    | DepositAmountChanged String
    | WithdrawalAmountChanged String
    | DepositClicked TokenValue
    | WithdrawClicked TokenValue
    | UserEthBalanceFetched (Result Http.Error TokenValue)
    | UserDerivedEthBalanceFetched (Result Http.Error TokenValue)
    | DerivedEthRedeemableFetched (Result Http.Error DethGenerated.CalculateRedemptionValue)
    | ApproveTokenSpend
    | DepositSigned (Result String TxHash)
    | WithdrawSigned (Result String TxHash)
    | FetchUserEthBalance
    | FetchUserDerivedEthBalance
    | DerivedEthIssuanceDetailFetched (Result Http.Error DethGenerated.CalculateIssuanceAmount)
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
    | FetchFryBalances
    | FryBalancesFetched (Result Http.Error ( HttpProvider, Address, AddressDict TokenValue ))
    | SetMouseoverState MouseoverState
    | UpdateNow Time.Posix
    | AmountInputChanged String
    | UXBack
    | StartDeposit TokenValue
    | DoUnlock
    | DoDeposit TokenValue
    | DoClaimRewards
    | DoExitFromOldFarms (List Address)
    | DoExit
    | StartWithdraw TokenValue
    | DoWithdraw TokenValue
    | DepositOrWithdrawSigned DepositOrWithdraw TokenValue (Result String TxHash)
    | StakingInfoFetched (Result Http.Error ( UserStakingInfo, Float ))
    | OldStakingBalanceFetched Address (Result Http.Error TokenValue)
    | ApyFetched (Result Http.Error Float)
    | RefetchStakingInfoOrApy
    | Navigate Route
    | WalletResponse (Result WalletConnectErr UserInfo)
    | ChainSwitchResponse (Result TxErr ())
    | TxSendResponse (Result TxErr TxHash)
    | FetchFarmingPeriodEnd
    | FarmingPeriodEndFetched (Result Http.Error BigInt)
    | DethProfitFetched (Result Http.Error TokenValue)
    | DethTVLFetched (Result Http.Error TokenValue)
    | DethSupplyFetched (Result Http.Error TokenValue)



-- | IssuedEventReceived (Event (Result Json.Decode.Error DethIssuedEventData))


type DethMode
    = DethDashboard
    | DethStats


type alias ComposedTreasuryBalance =
    List (Maybe TokenValue)


type alias PriceValue =
    { ethPrice : Float
    }


type alias UserInfo =
    { address : Address
    , balance : TokenValue
    , chain : Chain
    , xDaiStatus : XDaiStatus
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
    { dEthUserInfo : DEthUserInfo
    , dEthDepositInfo : DEthDepositInfo
    , dEthWithdrawInfo : DEthWithdrawInfo
    }


type alias DEthUserInfo =
    { ethBalance : TokenValue
    , dEthBalance : TokenValue
    , dEthAllowance : TokenValue
    }


type alias DEthDepositInfo =
    { actualCollateralAdded : TokenValue
    , depositFee : FeePair
    , tokensIssued : TokenValue
    }


type alias DEthWithdrawInfo =
    { totalCollateralRedeemed : TokenValue
    , redeemFee : FeePair
    , collateralReturned : TokenValue
    }


type alias DepositOrWithdrawUXModel =
    Maybe ( DepositOrWithdraw, AmountUXModel )


type alias AmountUXModel =
    { amountInput : String
    }


type alias FeePair =
    { protocolFee : TokenValue
    , automationFee : TokenValue
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
    | NetworkReady
    | Connecting
    | Active UserInfo


type alias VoteBarBlock =
    { x : Int
    , width : Int
    , colorRgb : ( Float, Float, Float )
    , address : Address
    , amount : TokenValue
    }


type InputValidationError
    = InputLessThan
    | InputGreaterThan
    | InputInvalid


type Chain
    = XDai
    | Eth
    | BSC


type alias ChainConfig =
    { chain : Chain

    -- , contract : Address
    -- , startScanBlock : Int
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


type TxErr
    = UserRejected
    | OtherErr String


type XDaiStatus
    = XDaiStandby
    | WaitingForApi
    | WaitingForBalance


type alias DethIssuedEventData =
    { receiver : Address
    , suppliedCollateral : TokenValue
    , protocolFee : TokenValue
    , automationFee : TokenValue
    , actualCollateralAdded : TokenValue
    , accreditedCollateral : TokenValue
    , tokensIssued : TokenValue
    }
