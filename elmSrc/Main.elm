-- Copyright 2017 True Ghiassi true@ghiassitrio.co.uk
-- This file is part of Whatthestudentsthink.
-- Whatthestudentsthink is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- Whatthestudentsthink is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Whatthestudentsthink.  If not, see
-- <http://www.gnu.org/licenses/>.


module Main exposing (..)

{-| Provides the main script for the home page of whatthestudentsthink.uk.
It provides the buttons and menus for choosing the data to display, and the
chart itself.
-}

import Data exposing (overallUniCodes, subjectCodes)
import DataTypes as D
    exposing
        ( AxesConfigType(..)
        , ChartMode(..)
        , DataChooserMode(..)
        , Model
        , Msg(..)
        )
import Dict exposing (Dict, keys, values)
import FreshData exposing (freshData)
import Html exposing (program)
import Update
import View


main : Program Never Model Msg
main =
    program
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = \_ -> Sub.none
        }


{-| The initial state of the app. The chart displayed initially is the
overall comparison chart for all universities on question 27 in the survey,
which is the overall satisfaction question.
-}
initModel : Model
initModel =
    { data = []
    , chartMode = Overall
    , pageLoading = True
    , getRequestErr = Nothing
    , axesConfig = UniVsA
    , fatalErr = Nothing
    , chooserMode = FoldedUp
    , questions = [ 27 ]
    , unis = keys overallUniCodes
    , subjectSearchIndex = Update.makeSearchIndex subjectCodes
    , uniSearchIndex = Update.makeSearchIndex overallUniCodes
    , subjectSearchResults = []
    , uniSearchResults = []
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, freshData initModel )
