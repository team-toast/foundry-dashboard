module Farm.State exposing (..)

import Common.Msg exposing (MsgDown, MsgUp)
import Common.Types exposing (..)
import Eth.Types exposing (Address)
import Farm.Types exposing (..)
import Wallet exposing (Wallet)


init : Maybe UserInfo -> ( Model, Cmd Msg )
init maybeUserInfo =
    ( { userBalanceInfo = Nothing
      }
    , case maybeUserInfo of
        Just userInfo ->
            fetchUserBalanceInfoCmd userInfo.address

        Nothing ->
            Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        NoOp ->
            justModelUpdate prevModel

        MsgUp msgUp ->
            UpdateResult
                prevModel
                Cmd.none
                [ msgUp ]


runMsgDown : MsgDown -> Model -> UpdateResult
runMsgDown msg prevModel =
    case msg of
        Common.Msg.UpdateWallet newWallet ->
            let
                newModel =
                    { prevModel | userBalanceInfo = Nothing }

                cmd =
                    case Wallet.userInfo newWallet of
                        Just userInfo ->
                            fetchUserBalanceInfoCmd userInfo.address

                        Nothing ->
                            Cmd.none
            in
            UpdateResult
                newModel
                cmd
                []


fetchUserBalanceInfoCmd : Address -> Cmd Msg
fetchUserBalanceInfoCmd userAddress =
    Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
