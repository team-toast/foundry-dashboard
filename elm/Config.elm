module Config exposing (..)

import BigInt exposing (BigInt)
import Eth.Types exposing (Address)
import Eth.Utils
import Set exposing (Set)
import Time
import TokenValue exposing (TokenValue)


testMode =
    False


httpProviderUrl : String
httpProviderUrl =
    if testMode then
        ganacheHttpProviderUrl

    else
        mainnetHttpProviderUrl


mainnetHttpProviderUrl : String
mainnetHttpProviderUrl =
    "https://23eb406fad764a70987ba5e619459917.eth.rpc.rivet.cloud/"


ganacheHttpProviderUrl : String
ganacheHttpProviderUrl =
    "http://localhost:8545"


daiContractAddress : Address
daiContractAddress =
    if testMode then
        Eth.Utils.unsafeToAddress "0xCfEB869F69431e42cdB54A4F4f105C19C080A601"

    else
        Eth.Utils.unsafeToAddress "0x6B175474E89094C44Da98b954EedeAC495271d0F"


erc20BalanceFetchBatchContractAddress : Address
erc20BalanceFetchBatchContractAddress =
    Eth.Utils.unsafeToAddress "0xb1F8e55c7f64D203C1400B9D8555d050F94aDF39"


fryContractAddress : Address
fryContractAddress =
    Eth.Utils.unsafeToAddress "0x6c972b70c533E2E045F333Ee28b9fFb8D717bE69"


etherscanBaseUrl : String
etherscanBaseUrl =
    "https://etherscan.io/address/"


teamToastMultiSigAddress : Address
teamToastMultiSigAddress =
    Eth.Utils.unsafeToAddress "0xA21510518cbF2627bb99966eA413ccf9F5b80f83"


treasuryForwarderAddress : Address
treasuryForwarderAddress =
    Eth.Utils.unsafeToAddress "0x93fE7D1d24bE7CB33329800ba2166f4D28Eaa553"


bucketSaleAddress : Address
bucketSaleAddress =
    Eth.Utils.unsafeToAddress "0x30076fF7436aE82207b9c03AbdF7CB056310A95A"


stakingContractAddress : Address
stakingContractAddress =
    Eth.Utils.unsafeToAddress "0x3bbAACCA406e83cA8D2d92f59bd8728740BD7Ff0"


stakingLiquidityContractAddress : Address
stakingLiquidityContractAddress =
    Eth.Utils.unsafeToAddress "0xcD1d5fF929E2B69BBD351CF31057E9a70eC76291"


stakingScriptsAddress : Address
stakingScriptsAddress =
    Eth.Utils.unsafeToAddress "0xa939728f9cdCdc4EEA16bdF3Aff03AB27036f4c7"


urlToLiquidityPool : String
urlToLiquidityPool =
    "https://info.uniswap.org/pair/0xcD1d5fF929E2B69BBD351CF31057E9a70eC76291"


forbiddenJurisdictionCodes : Set String
forbiddenJurisdictionCodes =
    Set.fromList [ "US" ]


farmingPeriodEnds : Int
farmingPeriodEnds =
    1607702381


bucketSaleTokensPerBucket : Int
bucketSaleTokensPerBucket =
    30000


uniswapGraphQL : String
uniswapGraphQL =
    "https://cors-anywhere.herokuapp.com/https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v2"


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
