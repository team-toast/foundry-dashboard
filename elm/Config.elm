module Config exposing (..)

import BigInt exposing (..)
import Dict exposing (..)
import Eth.Types exposing (Address)
import Eth.Utils
import Set exposing (Set)
import Time
import TokenValue exposing (TokenValue)
import Types exposing (ChainConfigs, ChainId)


ethChainId : ChainId
ethChainId =
    1


bscChainId : ChainId
bscChainId =
    56


testMode =
    False


displayProfileBreakpoint : Int
displayProfileBreakpoint =
    1150


ethereumProviderUrl : String
ethereumProviderUrl =
    "https://23eb406fad764a70987ba5e619459917.eth.rpc.rivet.cloud/"



-- "https://mainnet.infura.io/v3/429e19ab9d27496581835fe705ac4702"


arbitrumProviderUrl : String
arbitrumProviderUrl =
    "https://arbitrum-mainnet.infura.io/v3/429e19ab9d27496581835fe705ac4702"


polygonProviderUrl : String
polygonProviderUrl =
    "https://polygon-mainnet.infura.io/v3/429e19ab9d27496581835fe705ac4702"


nodeUrl : ChainId -> ChainConfigs -> String
nodeUrl chainId chainConfigs =
    if testMode then
        testModeHttpProviderUrl

    else
        case chainConfigs |> Dict.toList of
            [] ->
                ethereumProviderUrl

            _ ->
                chainConfigs
                    |> Dict.get chainId
                    |> Maybe.map
                        .nodeUrl
                    |> Maybe.withDefault ethereumProviderUrl


testModeHttpProviderUrl : String
testModeHttpProviderUrl =
    "https://mainnet.infura.io/v3/429e19ab9d27496581835fe705ac4702"


ethereumDaiContractAddress : Address
ethereumDaiContractAddress =
    Eth.Utils.unsafeToAddress "0x6B175474E89094C44Da98b954EedeAC495271d0F"


daiContractAddress : ChainId -> Maybe Address
daiContractAddress chainId =
    if chainId == ethChainId then
        if testMode then
            Just <| Eth.Utils.unsafeToAddress "0xCfEB869F69431e42cdB54A4F4f105C19C080A601"

        else
            Just ethereumDaiContractAddress

    else if chainId == bscChainId then
        Just <| Eth.Utils.unsafeToAddress "0x1AF3F329e8BE154074D8769D1FFa4eE058B1DBc3"

    else
        Nothing


ethErc20BalanceFetchBatchContractAddress : Address
ethErc20BalanceFetchBatchContractAddress =
    Eth.Utils.unsafeToAddress "0xb1F8e55c7f64D203C1400B9D8555d050F94aDF39"


ethLPBalanceFetchBatchContractAddress : Address
ethLPBalanceFetchBatchContractAddress =
    Eth.Utils.unsafeToAddress "0xb1F8e55c7f64D203C1400B9D8555d050F94aDF39"


polyLPBalanceFetchBatchContractAddress : Address
polyLPBalanceFetchBatchContractAddress =
    Eth.Utils.unsafeToAddress "0xfdb05419a14fa35dd06b5af13e5ae01edc93243b"


arbErc20BalanceFetchBatchContractAddress : Address
arbErc20BalanceFetchBatchContractAddress =
    Eth.Utils.unsafeToAddress "0x6b88B564EfC6fcE6515E73B0b1DAb5c4f10a1054"


polyErc20BalanceFetchBatchContractAddress : Address
polyErc20BalanceFetchBatchContractAddress =
    Eth.Utils.unsafeToAddress "0xe698841cd03a580e28611281739392a27b42b848"


ethereumFryContractAddress : Address
ethereumFryContractAddress =
    Eth.Utils.unsafeToAddress "0x6c972b70c533E2E045F333Ee28b9fFb8D717bE69"


arbitrumOneFryContractAddress : Address
arbitrumOneFryContractAddress =
    Eth.Utils.unsafeToAddress "0x633A3d2091dc7982597A0f635d23Ba5EB1223f48"


arbitrumOneGFryContractAddress : Address
arbitrumOneGFryContractAddress =
    Eth.Utils.unsafeToAddress "0x365Db53EEB009b447b0E5A95e2523596E074d2FE"


polyFryContractAddress : Address
polyFryContractAddress =
    Eth.Utils.unsafeToAddress "0xc9BAA8cfdDe8E328787E29b4B078abf2DaDc2055"


polyFryPosContractAddress : Address
polyFryPosContractAddress =
    Eth.Utils.unsafeToAddress "0x48d3a72230e65380f63a05ee41a7be31773c44b4"


polyGFryContractAddress : Address
polyGFryContractAddress =
    Eth.Utils.unsafeToAddress "0xC86c63E5681254Dd8DF64e69fB29eAD5a23dA461"


ethUniswapFryWethContractAddress : Address
ethUniswapFryWethContractAddress =
    Eth.Utils.unsafeToAddress "0xcd1d5ff929e2b69bbd351cf31057e9a70ec76291"


ethBalancerFryDaiDethContractAddress : Address
ethBalancerFryDaiDethContractAddress =
    Eth.Utils.unsafeToAddress "0x04a1f9f9fe8910a27972e15d5da3bf79075fefbb"


ethBalancerWstaWethAaveUniYfiFryContractAddress : Address
ethBalancerWstaWethAaveUniYfiFryContractAddress =
    Eth.Utils.unsafeToAddress "0xe6cb1c3a212001d02706ef93ea0a87b35b36d016"


blockExplorerUrl : ChainId -> ChainConfigs -> Maybe String
blockExplorerUrl chainId chainConfigs =
    chainConfigs
        |> Dict.get chainId
        |> Maybe.map .explorerUrl


teamToastMultiSigAddress : Address
teamToastMultiSigAddress =
    Eth.Utils.unsafeToAddress "0xF7396C708Ad9127B6684b7fd690083158d2ebdE5"


mainTreasuryAddress : Address
mainTreasuryAddress =
    Eth.Utils.unsafeToAddress "0x93fE7D1d24bE7CB33329800ba2166f4D28Eaa553"


treasuryAddresses : List Address
treasuryAddresses =
    [ mainTreasuryAddress
    , Eth.Utils.unsafeToAddress "0xecB864Bfed3260388d1a7f8F182053F406074C5a"
    , Eth.Utils.unsafeToAddress "0x9C59BAD7674AC1b9485Ede5c19af3B42F0516392"
    ]


bucketSaleAddress : Address
bucketSaleAddress =
    Eth.Utils.unsafeToAddress "0x30076fF7436aE82207b9c03AbdF7CB056310A95A"


oldStakingContractAddresses : List Address
oldStakingContractAddresses =
    [ Eth.Utils.unsafeToAddress "0xa2a4ef5722b198a139dd93b1d420769261808fd1"
    , Eth.Utils.unsafeToAddress "0x3bbAACCA406e83cA8D2d92f59bd8728740BD7Ff0"
    ]


stakingContractAddress : Address
stakingContractAddress =
    Eth.Utils.unsafeToAddress "0x773617551B0A6455B366a3D706629725E34AF684"


stakingLiquidityContractAddress : Address
stakingLiquidityContractAddress =
    Eth.Utils.unsafeToAddress "0x04a1f9f9fe8910a27972e15d5da3bf79075fefbb"


stakingScriptsAddress : Address
stakingScriptsAddress =
    Eth.Utils.unsafeToAddress "0xffcfcd407556b00eb17992ef9b5e926d7faaa6c3"


liquidityPoolUrl : String
liquidityPoolUrl =
    "https://pools.balancer.exchange/#/pool/0x04a1f9f9fe8910a27972e15d5da3bf79075fefbb/shares"


forbiddenJurisdictionCodes : Set String
forbiddenJurisdictionCodes =
    Set.fromList [ "US" ]


bucketSaleTokensPerBucket : Int
bucketSaleTokensPerBucket =
    30000


uniswapGraphQL : String
uniswapGraphQL =
    "https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v2"


saleStarted : Int
saleStarted =
    1592568000000


bucketSaleBucketInterval : Time.Posix
bucketSaleBucketInterval =
    Time.millisToPosix <| 1000 * 60 * 60 * 7


fryTotalSupply : Int
fryTotalSupply =
    100000000


teamToastAddress1 : Address
teamToastAddress1 =
    Eth.Utils.unsafeToAddress "0xe5dde1cc679184fc420e6f92e0bd8c81e41d25e1"


teamToastAddress2 : Address
teamToastAddress2 =
    Eth.Utils.unsafeToAddress "0x91227d115d036a721f6455b4e201b2f74576da43"


teamToastAddress3 : Address
teamToastAddress3 =
    Eth.Utils.unsafeToAddress "0x16af660a19567d273842c826b9b4f4992b5b4626"


burnAddress : Address
burnAddress =
    Eth.Utils.unsafeToAddress "0x0000000000000000000000000000000000000001"


balancerPermafrostPool : Address
balancerPermafrostPool =
    Eth.Utils.unsafeToAddress "0x5277a42ef95eca7637ffa9e69b65a12a089fe12b"


dethContractAddress : Address
dethContractAddress =
    Eth.Utils.unsafeToAddress "0x51863Ec92BA14ede7B17fb2B053145C90E215A57"


dethGulperAddress : Address
dethGulperAddress =
    Eth.Utils.unsafeToAddress "0x1192AF4C49b88C7d102b1779E6E7d9A503366C28"


saleEndTime : Time.Posix
saleEndTime =
    Time.millisToPosix
        (1642968000 * 1000)


userDethWarningThresholdRatio : Float
userDethWarningThresholdRatio =
    0.35


dethStartScanBlock : Int
dethStartScanBlock =
    12799856
