module AddressDict exposing (AddressDict, empty, filter, fromList, get, insert, keys, map, member, toList, union, update, merge, foldl)

import Dict exposing (..)
import Eth.Types exposing (Address)
import Eth.Utils


type AddressDict a = 
    AddressDict (Dict String a)


unwrap : AddressDict a -> Dict String a
unwrap addressDict_ =
    case addressDict_ of
        AddressDict addressDict -> addressDict
        

translate :
    (Address -> a)
    -> String 
    -> a
translate fn address = fn (deterministicStringToAddress address)


wrapFunc : ((String -> x) -> Dict String a -> Dict String b) -> (Address -> x) -> AddressDict a -> AddressDict b
wrapFunc originalFunc inputFunc addressDict =
    (unwrap addressDict) 
    |> originalFunc (translate inputFunc) 
    |> AddressDict


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
    unwrap addressDict_ |> Dict.get (addressToDeterministicString address)

map : (Address -> a -> b) -> AddressDict a -> AddressDict b
map = wrapFunc Dict.map


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



merge :  (Address -> a -> result -> result)
  -> (Address -> a -> b -> result -> result)
  -> (Address -> b -> result -> result)
  -> AddressDict a
  -> AddressDict b
  -> result
  -> result
merge leftStep_ bothStep_ rightStep_ leftAddressDict_ rightAddressDict_ initialResult =
    Dict.merge 
        (translate leftStep_)
        (translate bothStep_)
        (translate rightStep_)
        (unwrap leftAddressDict_) 
        (unwrap rightAddressDict_) 
        initialResult

foldl : (Address -> v -> b -> b) -> b -> AddressDict v -> b
foldl func acc addressDict_ =
    case addressDict_ of
        AddressDict addressDict ->
            addressDict
                |> Dict.foldl (translate func) acc

toList : AddressDict a -> List ( Address, a )
toList addressDict_ =
    case addressDict_ of
        AddressDict addressDict ->
            addressDict
                |> Dict.toList
                |> List.map (Tuple.mapFirst deterministicStringToAddress)


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : AddressDict a -> AddressDict a -> AddressDict a
union dict1_ dict2_ =
    case ( dict1_, dict2_ ) of
        ( AddressDict dict1, AddressDict dict2 ) ->
            AddressDict <| Dict.union dict1 dict2


member : Address -> AddressDict a -> Bool
member address dict_ =
    Dict.member (addressToDeterministicString address) (unwrap dict_)

addressToDeterministicString : Address -> String
addressToDeterministicString =
    Eth.Utils.addressToString >> String.toLower


deterministicStringToAddress : String -> Address
deterministicStringToAddress =
    Eth.Utils.unsafeToAddress
