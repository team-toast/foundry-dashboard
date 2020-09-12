module Config exposing (..)

import BigInt exposing (BigInt)
import Eth.Types exposing (Address)
import Eth.Utils
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

etherscanBaseUrl: String
etherscanBaseUrl = "https://etherscan.io/address/"

teamToastMultiSigAddress: Address
teamToastMultiSigAddress = Eth.Utils.unsafeToAddress "0xE5dDe1cc679184fc420E6f92e0Bd8C81E41D25e1"


treasuryForwarderAddress: Address
treasuryForwarderAddress = Eth.Utils.unsafeToAddress "0x93fE7D1d24bE7CB33329800ba2166f4D28Eaa553"


fryTokenAddress : Address
fryTokenAddress = Eth.Utils.unsafeToAddress "0x6c972b70c533E2E045F333Ee28b9fFb8D717bE69"


bucketSaleAddress : Address
bucketSaleAddress = Eth.Utils.unsafeToAddress "0x30076fF7436aE82207b9c03AbdF7CB056310A95A"