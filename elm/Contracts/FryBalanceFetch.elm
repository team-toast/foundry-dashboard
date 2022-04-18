module Contracts.FryBalanceFetch exposing (..)

import AddressDict exposing (AddressDict)
import Config
import Contracts.Generated.ERC20BalanceFetchBatch as BulkBalanceFetch
import Eth
import Eth.Types exposing (..)
import Http
import Task
import TokenValue exposing (TokenValue)
import Types exposing (BalanceSet)
import Eth.Net exposing (NetworkId(..))
import List.Extra exposing (uniqueBy)
import Eth.Utils exposing (addressToString)
import Types exposing (CrossChainFryBalanceTracker, Msg)

accumulateFetches :
    AddressDict (CrossChainFryBalanceTracker)
    -> List (Cmd Msg)
accumulateFetches dict =
    -- transform the dict into 3 lists of addresses 
    -- create a cmd to fetch each list
    let
        filterBalancesToList fn =
            dict
            |> AddressDict.map  (\_ i -> fn i)
            |> AddressDict.filter (\_ i -> Maybe.Nothing == i)
            |> AddressDict.toList 
    in
        [ filterBalancesToList .ethFryBalance
        , filterBalancesToList .arbFryBalance
        , filterBalancesToList .arbGFryBalance ]

    

    -- dict 
    -- |> AddressDict.toList
    -- |> List.map (\(addr, tracker) ->
    --     ([
    --         case tracker.ethBalance of
    --             Nothing ->
    --                 Debug.todo "fetch"
    --             Just _ ->
    --                 Cmd.none
    --         , case tracker.arbBalance of
    --             Nothing ->
    --                 Debug.todo "fetch"
    --             Just _ ->
    --                 Cmd.none
    --         , case tracker.arbGFryBalance of
    --             Nothing ->
    --                 Debug.todo "fetch"
    --             Just _ ->
    --                 Cmd.none 
    --     ]
    --     |> Cmd.batch))
