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


module View exposing (view)

{-| It provies the main 'view' function for the app.
-}

import Chart
import DataChooser exposing (dataChooserButtons)
import DataTypes
    exposing
        ( AxesConfigType(..)
        , ChartMode(..)
        , Model
        , Msg(..)
        )
import Html
    exposing
        ( Html
        , button
        , div
        , p
        , span
        , text
        )
import Html.Attributes exposing (class)


{-| Creates some Html to display, given the global state contained in the
model. Most of it is the chart.
-}
view : Model -> Html Msg
view model =
    div [] <|
        [ div
            [ class "topDiv"
            ]
            [ div
                [ class "dataChooserDiv"
                ]
                (dataChooserButtons model)
            , explainConfidenceIntervals
            ]
        , div [ class "chartCaption" ] (Chart.makeCaption model)
        , chartOrMsg model
        ]


{-| The explanations of the confidence intervals.
-}
explainConfidenceIntervals : Html Msg
explainConfidenceIntervals =
    div
        [ class "explainP" ]
        [ text """The dot gives the percentage of students who answered "Mostly agree" or "Definitely agree".  The line gives the """
        , Html.a
            [ Html.Attributes.href "https://www.officeforstudents.org.uk/advice-and-guidance/student-information-and-data/national-student-survey-nss/questions-about-the-nss-data/"
            ]
            [ text "95% confidence interval" ]
        , text "."
        ]


{-| It displays the chart if possible, and an error message if not.
-}
chartOrMsg : Model -> Html Msg
chartOrMsg model =
    case ( errorMsg model, model.fatalErr ) of
        ( Just msg, Nothing ) ->
            div [ class "errMsg" ] [ text msg ]

        ( Nothing, Nothing ) ->
            div [] <| Chart.chart model

        ( _, Just err ) ->
            div []
                [ text <|
                    String.concat
                        [ "There was an internal error, so the chart cannot be displayed: "
                        , err
                        ]
                ]


{-| Checks if all is as it should be, and makes a human-readable error
message if not.
-}
errorMsg : Model -> Maybe String
errorMsg { pageLoading, axesConfig, chartMode, data, getRequestErr, unis, questions } =
    case
        ( (pageLoading
        , getRequestErr
        , unis)
        , (chartMode
        , questions
        , data)
        , dataCorrectLength axesConfig data unis questions
        )
    of
        ( (True, _, _), (_, _, _), _ ) ->
            Just <| "Loading data.  Please wait..."

        ( (_, Just _, _), (_, _, _), _ ) ->
            Just "Chart not available.  Check internet connection."

        ( (_, _, []), (_, _, _), _ ) ->
            Just <| noSelectErrorMsg UniErr

        ( (_, _, _), (BySubject Nothing, _, _), _ ) ->
            Just <| noSelectErrorMsg SubjectErr

        ( (_, _, _), (_, [], _), _ ) ->
            Just <| noSelectErrorMsg QuestionErr

        ( (_, _, _), (_, _, []), _ ) ->
            Just "No chart because no data for this university and subject."

        ( (_, _, _), (_, _, _), False ) ->
            Just <| "No chart because not enough data for these universities and subjects. Check to see if a university or subject in red is selected."

        _ ->
            Nothing


{-| The server returns a list of lists of integers, which is the data to display
in the chart. As a quick easy error check, this function makes sure it is the
correct length.
-}
dataCorrectLength :
    AxesConfigType
    -> List ( Int, Int, Int )
    -> List Int
    -> List Int
    -> Bool
dataCorrectLength axesConfig data unis questions =
    case axesConfig of
        UniVsA ->
            List.length data == List.length unis

        QVsA ->
            List.length data == List.length questions


type SelectErr
    = AnswerErr
    | QuestionErr
    | UniErr
    | SubjectErr


{-| This makes the error message to display if the user has not selected
enough options for the chart to be displayed.
-}
noSelectErrorMsg : SelectErr -> String
noSelectErrorMsg axisType =
    String.concat
        [ "No "
        , noSelectWords axisType
        , " selected."
        ]


{-| Used to make an error message when not enough data has been selected to
produce the chart.
-}
noSelectWords : SelectErr -> String
noSelectWords axisType =
    case axisType of
        UniErr ->
            "universities"

        QuestionErr ->
            "questions"

        AnswerErr ->
            "answers"

        SubjectErr ->
            "subjects"
