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
                "https://23eb406fad764a70987ba5e619459917.eth.rpc.rivet.cloud/"

        BSC ->
            "https://bsc-dataseed1.binance.org/"

        Matic ->
            "https://rpc-mainnet.maticvigil.com"

        -- XDai ->
        --     "https://xdai-archive.blockscout.com"
            
        


testModeHttpProviderUrl : String
testModeHttpProviderUrl =
    "http://localhost:8545"


daiContractAddress : Chain -> Address
daiContractAddress chain =
    Eth.Utils.unsafeToAddress <|
        case chain of
            Eth ->
                if testMode then
                    "0xCfEB869F69431e42cdB54A4F4f105C19C080A601"

                else
                    "0x6B175474E89094C44Da98b954EedeAC495271d0F"

            BSC ->
                "0x1AF3F329e8BE154074D8769D1FFa4eE058B1DBc3"
            
            Matic ->
                "0x8f3Cf7ad23Cd3CaDbD9735AFf958023239c6A063"

            -- XDai ->
            --     ""


erc20BalanceFetchBatchContractAddress : Address
erc20BalanceFetchBatchContractAddress =
    Eth.Utils.unsafeToAddress "0xb1F8e55c7f64D203C1400B9D8555d050F94aDF39"


fryContractAddress : Chain -> Address
fryContractAddress chain =
    Eth.Utils.unsafeToAddress <|
        case chain of
            Eth ->
                "0x6c972b70c533E2E045F333Ee28b9fFb8D717bE69"

            BSC ->
                "0xc04e039ae8587e71f8024b36d630f841cc2106cc"
            
            Matic ->
                "0xc9BAA8cfdDe8E328787E29b4B078abf2DaDc2055"

            -- XDai ->
            --     ""


addressExplorerUrl : Chain -> String
addressExplorerUrl chain =
    case chain of
        Eth ->
            "https://etherscan.io/address/"

        BSC ->
            "https://bscscan.com/address/"

        Matic ->
            "https://explorer-mainnet.maticvigil.com/address/"


teamToastMultiSigAddress : Address
teamToastMultiSigAddress =
    Eth.Utils.unsafeToAddress "0xF7396C708Ad9127B6684b7fd690083158d2ebdE5"


treasuryForwarderAddress : Address
treasuryForwarderAddress =
    Eth.Utils.unsafeToAddress "0x93fE7D1d24bE7CB33329800ba2166f4D28Eaa553"


hotTreasuryAddress : Address
hotTreasuryAddress =
    Eth.Utils.unsafeToAddress "0x9C59BAD7674AC1b9485Ede5c19af3B42F0516392"


bucketSaleAddress : Address
bucketSaleAddress =
    Eth.Utils.unsafeToAddress "0x30076fF7436aE82207b9c03AbdF7CB056310A95A"


stakingContractAddress : Chain -> Address
stakingContractAddress chain =
    Eth.Utils.unsafeToAddress <|
        case chain of
            Eth ->
                "0xa2a4ef5722b198a139dd93b1d420769261808fd1"

            BSC ->
                "0xC13aa70E9437349834B1B093f8C5922fdEF6496B"

            Matic ->
                "0xd3dA0fCbB32aF2B03c0B3395CA3bc64a0ef01CFf"


stakingLiquidityContractAddress : Chain -> Address
stakingLiquidityContractAddress chain =
    Eth.Utils.unsafeToAddress <|
        case chain of
            Eth ->
                "0xcD1d5fF929E2B69BBD351CF31057E9a70eC76291"

            BSC ->
                "0xe71c65eb18fab7c8dd99598973fd8fa18570fb01"

            Matic ->
                "0x07af07c014E221f52F68cED635E5060873b9461B"


stakingQueryScriptsAddress : Chain -> Address
stakingQueryScriptsAddress chain =
    Eth.Utils.unsafeToAddress <|
        case chain of
            Eth ->
                "0x929378d7E8D9df3aE96835bac4DaD7dfc5741beA"

            BSC ->
                "0x6cefd06178B924b550F28bD9B9546338337d3B31"

            Matic ->
                "0x8114c2EA3Aa9B0Adf501C7a63EFea2c2c6E4c0fC"



stakingPricePairAddress : Chain -> Address
stakingPricePairAddress chain =
    Eth.Utils.unsafeToAddress <|
        case chain of
            Eth ->
                -- DAI/WETH
                "0xa478c2975ab1ea89e8196811f51a7b7ade33eb11"

            BSC ->
                -- BUSD/BNB
                "0x1b96b92314c44b159149f7e0303511fb2fc4774f"

            Matic ->
                -- WMATIC/USDC
                -- note that this pair is reversed. This will matter when the query contract does a less stupid price calculation
                "0x6e7a5fafcec6bb1e78bae2a1f0b612012bf14827"



urlToLiquidityPool : Chain -> String
urlToLiquidityPool chain =
    case chain of
        Eth ->
            "https://app.uniswap.org/#/add/v2/0x6c972b70c533E2E045F333Ee28b9fFb8D717bE69/ETH"

        BSC ->
            "https://exchange.pancakeswap.finance/#/add/BNB/0xc04e039aE8587e71f8024b36d630f841cc2106CC"

        Matic ->
            "https://quickswap.exchange/#/add/0x0d500b1d8e8ef31e21c99d1db9a6444d3adf1270/0xc9baa8cfdde8e328787e29b4b078abf2dadc2055"


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
    Eth.Utils.unsafeToAddress "0x5420dFecFaCcDAE68b406ce96079d37743Aa11Ae"


ethAddress : Address
ethAddress =
    Eth.Utils.unsafeToAddress "0xEeeeeEeeeEeEeeEeEeEeeEEEeeeeEeeeeeeeEEeE"
