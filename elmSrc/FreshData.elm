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


module FreshData exposing (freshData)

{-| For making requests to the server for fresh data.
-}

import DataTypes
    exposing
        ( ChartMode(..)
        , GetData
        , Model
        , Msg(..)
        )
import Http
import Json.Decode as Decode
import Json.Encode as Encode


{-| It sends a PUT request to the server (unless there is no subject chosen).
It uses the selected subject, universities and questions from the model to
make the request. The selection is sent to the server as lists of integer
codes encoded to Json.

The response is Json, a nested list of Ints, each sublist containing three
elements, which are the lower bound of the 95% confidence interval, the score
and the upper bound.

-}
freshData : Model -> Cmd Msg
freshData model =
    case model.chartMode of
        BySubject Nothing ->
            Cmd.none

        BySubject (Just subj) ->
            Http.send DataRequest
                (makePutRequest (SoSubject subj) model.unis model.questions)

        Overall ->
            Http.send DataRequest
                (makePutRequest SoOverall model.unis model.questions)


{-| Makes a GET request to the server. The arguments are:

    + subOrOverall: This says which data set to use.  If a subject has been
      selected then it is the by-subject worksheet from the data.  If a subject
      has not been selected, then it is the overall worksheet, with scores for
      each question across all subjects at each university.

    + unis: A list of the integer codes of the selected universities.

    + questions: A list of the integer codes of the selected questions.

-}
makePutRequest : SubjectOrOverall -> List Int -> List Int -> Http.Request GetData
makePutRequest subOrOverall unis questions =
    Http.request
        { method = "PUT"
        , headers = []
        , url =
            case subOrOverall of
                SoSubject _ ->
                    "nss2"

                SoOverall ->
                    "nss"
        , body = Http.jsonBody <| jsonRequest subOrOverall unis questions
        , expect = Http.expectJson decodeData
        , timeout = Nothing
        , withCredentials = False
        }


{-| Used to tell the function that constructs the GET request whether the
data is to come from the overall data set or the detailed per-subject
data set.
-}
type SubjectOrOverall
    = SoOverall
    | SoSubject Int


{-| Encodes the JSON for the GET request.
-}
jsonRequest : SubjectOrOverall -> List Int -> List Int -> Encode.Value
jsonRequest subOrOverall unis questions =
    case subOrOverall of
        SoSubject subject ->
            Encode.object
                [ ( "n2Subject", Encode.int subject )
                , ( "n2Unis", encodeList unis )
                , ( "n2Questions", encodeList questions )
                ]

        SoOverall ->
            Encode.object
                [ ( "nUnis", encodeList unis )
                , ( "nQuestions", encodeList questions )
                ]


{-| JSON encoder for a list of Ints.
-}
encodeList : List Int -> Encode.Value
encodeList list =
    Encode.list (List.map Encode.int list)


{-| Decodes the JSON given in response to the GET request to the server.
-}
decodeData : Decode.Decoder GetData
decodeData =
    Decode.map2 GetData
        (Decode.field "err" Decode.string)
        (Decode.field "result" (Decode.list (Decode.list Decode.int)))
