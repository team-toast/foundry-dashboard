module View.Attrs exposing (..)

import Helpers.Element exposing (DisplayProfile(..))


maxBarWidth : DisplayProfile -> Int
maxBarWidth dProfile =
    case dProfile of
        Desktop ->
            200

        Mobile ->
            125
