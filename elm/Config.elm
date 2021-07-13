module Config exposing (..)

import BigInt exposing (BigInt)
import Eth.Types exposing (Address)
import Eth.Utils
import Set exposing (Set)
import Time
import TokenValue exposing (TokenValue)
import Types exposing (Chain(..))


testMode =
    False


displayProfileBreakpoint : Int
displayProfileBreakpoint =
    1150


ethereumProviderUrl : String
ethereumProviderUrl =
    "https://23eb406fad764a70987ba5e619459917.eth.rpc.rivet.cloud/"


httpProviderUrl : Chain -> String
httpProviderUrl chain =
    mainnetHttpProviderUrl chain


mainnetHttpProviderUrl : Chain -> String
mainnetHttpProviderUrl chain =
    case chain of
        Eth ->
            if testMode then
                testModeHttpProviderUrl

            else
                ethereumProviderUrl

        BSC ->
            "https://bsc-dataseed1.binance.org/"

        XDai ->
            Debug.todo "xDai provider url"


testModeHttpProviderUrl : String
testModeHttpProviderUrl =
    "http://localhost:8545"


ethereumDaiContractAddress : Address
ethereumDaiContractAddress =
    Eth.Utils.unsafeToAddress "0x6B175474E89094C44Da98b954EedeAC495271d0F"


daiContractAddress : Chain -> Maybe Address
daiContractAddress chain =
    case chain of
        Eth ->
            if testMode then
                Just <| Eth.Utils.unsafeToAddress "0xCfEB869F69431e42cdB54A4F4f105C19C080A601"

            else
                Just ethereumDaiContractAddress

        BSC ->
            Just <| Eth.Utils.unsafeToAddress "0x1AF3F329e8BE154074D8769D1FFa4eE058B1DBc3"

        XDai ->
            Nothing


erc20BalanceFetchBatchContractAddress : Address
erc20BalanceFetchBatchContractAddress =
    Eth.Utils.unsafeToAddress "0xb1F8e55c7f64D203C1400B9D8555d050F94aDF39"


ethereumFryContractAddress : Address
ethereumFryContractAddress =
    Eth.Utils.unsafeToAddress "0x6c972b70c533E2E045F333Ee28b9fFb8D717bE69"


fryContractAddressForChain : Chain -> Maybe Address
fryContractAddressForChain chain =
    case chain of
        Eth ->
            Just ethereumFryContractAddress

        BSC ->
            Just <| Eth.Utils.unsafeToAddress "0xc04e039ae8587e71f8024b36d630f841cc2106cc"

        XDai ->
            Nothing


blockExplorerUrl : Chain -> String
blockExplorerUrl chain =
    case chain of
        Eth ->
            "https://etherscan.io/address/"

        BSC ->
            "https://bscscan.com/address/"

        XDai ->
            "https://blockscout.com/xdai/mainnet/"


teamToastMultiSigAddress : Address
teamToastMultiSigAddress =
    Eth.Utils.unsafeToAddress "0xF7396C708Ad9127B6684b7fd690083158d2ebdE5"


mainTreasuryAddress : Address
mainTreasuryAddress =
    Eth.Utils.unsafeToAddress "0x9C59BAD7674AC1b9485Ede5c19af3B42F0516392"


treasuryAddresses : List Address
treasuryAddresses =
    [ mainTreasuryAddress
    , Eth.Utils.unsafeToAddress "0xecB864Bfed3260388d1a7f8F182053F406074C5a"
    , Eth.Utils.unsafeToAddress "0x93fE7D1d24bE7CB33329800ba2166f4D28Eaa553"
    ]


bucketSaleAddress : Address
bucketSaleAddress =
    Eth.Utils.unsafeToAddress "0x30076fF7436aE82207b9c03AbdF7CB056310A95A"


oldStakingContractAddress : Address
oldStakingContractAddress =
    Eth.Utils.unsafeToAddress "0xa2a4ef5722b198a139dd93b1d420769261808fd1"


stakingContractAddress : Address
stakingContractAddress =
    Eth.Utils.unsafeToAddress <|
        Debug.todo "New staking contract address?"


stakingLiquidityContractAddress : Address
stakingLiquidityContractAddress =
    Eth.Utils.unsafeToAddress <|
        Debug.todo "0xcD1d5fF929E2B69BBD351CF31057E9a70eC76291"


stakingScriptsAddress : Address
stakingScriptsAddress =
    Eth.Utils.unsafeToAddress <|
        Debug.todo "0x929378d7E8D9df3aE96835bac4DaD7dfc5741beA"


stakingPricePairAddress : Address
stakingPricePairAddress =
    Debug.todo "0xa478c2975ab1ea89e8196811f51a7b7ade33eb11"


liquidityPoolUrl : String
liquidityPoolUrl =
    Debug.todo "https://app.uniswap.org/#/add/v2/0x6c972b70c533E2E045F333Ee28b9fFb8D717bE69/ETH"


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


derivedEthContractAddress : Address
derivedEthContractAddress =
    Eth.Utils.unsafeToAddress "0x51863Ec92BA14ede7B17fb2B053145C90E215A57"


saleEndTime : Time.Posix
saleEndTime =
    Time.millisToPosix
        (1642968000 * 1000)
