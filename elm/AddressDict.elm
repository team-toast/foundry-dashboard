module AddressDict exposing (AddressDict, empty, filter, fromList, get, insert, keys, map, toList, union, update)

import Dict exposing (Dict)
import Eth.Types exposing (Address)
import Eth.Utils


type AddressDict a
    = AddressDict (Dict String a)


empty : AddressDict a
empty =
    AddressDict Dict.empty


fromList : List ( Address, a ) -> AddressDict a
fromList list =
    list
        |> List.map (Tuple.mapFirst addressToDeterministicString)
        |> Dict.fromList
        |> AddressDict


get : Address -> AddressDict a -> Maybe a
get address addressDict_ =
    case addressDict_ of
        AddressDict addressDict ->
            addressDict
                |> Dict.get (addressToDeterministicString address)


map : (Address -> a -> b) -> AddressDict a -> AddressDict b
map func addressDict_ =
    case addressDict_ of
        AddressDict addressDict ->
            addressDict
                |> Dict.map
                    (\addressString ->
                        func (deterministicStringToAddress addressString)
                    )
                |> AddressDict


filter : (Address -> a -> Bool) -> AddressDict a -> AddressDict a
filter func addressDict_ =
    case addressDict_ of
        AddressDict addressDict ->
            addressDict
                |> Dict.filter
                    (\addressString ->
                        func (deterministicStringToAddress addressString)
                    )
                |> AddressDict


keys : AddressDict a -> List Address
keys addressDict_ =
    case addressDict_ of
        AddressDict addressDict ->
            addressDict
                |> Dict.keys
                |> List.map deterministicStringToAddress


insert : Address -> a -> AddressDict a -> AddressDict a
insert address val addressDict_ =
    case addressDict_ of
        AddressDict addressDict ->
            addressDict
                |> Dict.insert
                    (addressToDeterministicString address)
                    val
                |> AddressDict


update : Address -> (Maybe a -> Maybe a) -> AddressDict a -> AddressDict a
update address updateFunc addressDict_ =
    case addressDict_ of
        AddressDict addressDict ->
            addressDict
                |> Dict.update
                    (addressToDeterministicString address)
                    updateFunc
                |> AddressDict


toList : AddressDict a -> List ( Address, a )
toList addressDict_ =
    case addressDict_ of
        AddressDict addressDict ->
            addressDict
                |> Dict.toList
                |> List.map (Tuple.mapFirst deterministicStringToAddress)


union : AddressDict a -> AddressDict a -> AddressDict a
union dict1_ dict2_ =
    case ( dict1_, dict2_ ) of
        ( AddressDict dict1, AddressDict dict2 ) ->
            AddressDict <| Dict.union dict1 dict2


addressToDeterministicString : Address -> String
addressToDeterministicString =
    Eth.Utils.addressToString >> String.toLower


deterministicStringToAddress : String -> Address
deterministicStringToAddress =
    Eth.Utils.unsafeToAddress
