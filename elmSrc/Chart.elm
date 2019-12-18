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


module Chart exposing (chart, makeCaption)

{-| It creates the chart from the model. The chart is an SVG canvas with all
the elements drawn on it.
-}

import AxisLabels exposing (gapOnLeft, xAxisLabels, yAxisLabels)
import Data exposing (overallUniCodes, subjectCodes, uniCodes)
import DataTypes
    exposing
        ( AxesConfigType(..)
        , AxisType(..)
        , ChartMode(..)
        , Model
        , Msg(..)
        , plotScale
        , questionCodes
        , scale
        , yAxisType
        )
import Dict exposing (Dict, get, values)
import Html exposing (Html, div, i, p, span)
import List exposing (concat, indexedMap, length, map, maximum)
import Maybe.Extra exposing (combine)
import String exposing (join)
import Svg exposing (circle, svg, text)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , fill
        , preserveAspectRatio
        , r
        , viewBox
        , width
        )


{-| Given the model (the global state of the whole app), it creates a list
of Html elements that make up the chart. An empty list is produced if it was
not possible to create the chart.
-}
chart : Model -> List (Html Msg)
chart model =
    let
        yAxisTypel =
            yAxisType model.axesConfig

        yLabelNames =
            labelList model yAxisTypel

        widestXlabel =
            12

        widestYlabel =
            calcOffset yLabelNames

        points =
            concat <|
                indexedMap
                    (plotRow widestXlabel)
                    model.data

        xLabels =
            xAxisLabels model widestYlabel widestXlabel (length yLabelNames)

        maybeYLabels =
            yAxisLabels model widestYlabel widestXlabel

        viewX =
            widestYlabel + plotScale * 100 + 20

        viewY =
            widestXlabel + round scale * (length yLabelNames + 1)
    in
    case maybeYLabels of
        Just yLabels ->
            [ svg
                [ viewBox <|
                    join " " [ "0 0", String.fromInt viewX, String.fromInt viewY ]
                , preserveAspectRatio "xMidYMid meet"
                , width <| (String.fromFloat <| 0.27 * toFloat viewX) ++ "mm"
                ]
              <|
                concat [ xLabels, yLabels, points ]
            ]

        Nothing ->
            []


{-| It makes the caption for the chart. This is something like:

    Percentage of students studying <some subject> who agreed with
    <some survey question>.

or

    Percentage of students who agreed with <some survey question>.

or

    Percentages of students at <some university> who agreed with
    each survey question.

-}
makeCaption : Model -> List (Html Msg)
makeCaption model =
    case ( (model.axesConfig, model.chartMode), (model.questions, model.unis) ) of
        ( ( UniVsA, Overall) , ([ questionNum ], _) ) ->
            case get questionNum questionCodes of
                Nothing ->
                    []

                Just question ->
                    percentWhoAgreed question

        ( (UniVsA, Overall), ( _, _) ) ->
            []

        ( (UniVsA, BySubject Nothing), (_, _) ) ->
            []

        ( (UniVsA, BySubject (Just subjectNum)), ([ questionNum ], _) ) ->
            case ( get subjectNum subjectCodes, get questionNum questionCodes ) of
                ( Just subject, Just question ) ->
                    whoWereStudying question subject

                _ ->
                    []

        ( (UniVsA, BySubject _), (_, _) ) ->
            []

        ( (QVsA, Overall), (_, [ uniNum ]) ) ->
            case get uniNum overallUniCodes of
                Nothing ->
                    []

                Just uni ->
                    overallQVsA uni

        ( (QVsA, BySubject (Just subjectNum)), (_, [ uniNum ]) ) ->
            case ( get subjectNum subjectCodes, get uniNum uniCodes ) of
                ( Just subject, Just uni ) ->
                    bySubjectQVsA subject uni

                _ ->
                    []

        _ ->
            []


{-| Given a subject and a university name, it creates a caption for the
chart of the form:

    Percentage of students studying <some subject> at <some university>
    who agreed with each of the survey questions.

This is used for charts with the score for a particular subject and university
on the x-axis and the question text on the y-axis.

-}
bySubjectQVsA : String -> String -> List (Html Msg)
bySubjectQVsA subject uni =
    [ span [] [ text "Percentage of students studying " ]
    , i [] [ text subject ]
    , span [] [ text " at " ]
    , i [] [ text uni ]
    , span [] [ text " who agreed with each of the survey questions." ]
    ]


{-| Given the name of a university, it makes a caption for the chart like:

    Percentage of students at <name of university> who agreed with each
    of the survey questions.

This is used for charts with the score for all subjects at a university on
the x-axis, and the survey question text on the y-axis.

-}
overallQVsA : String -> List (Html Msg)
overallQVsA uni =
    [ span [] [ text "Percentage of students at " ]
    , i [] [ text uni ]
    , span [] [ text " who agreed with each of the survey questions." ]
    ]


{-| Given a survey question and a subject, this creates a caption for the
chart like this:

    Percentage of students studying <subject name> who agreed with
    <question text>.

This is for the charts with score for a particular subject on the x-axis
and university names on the y-axis.

-}
whoWereStudying : String -> String -> List (Html Msg)
whoWereStudying question subject =
    [ span [] [ text "Percentage of students studying " ]
    , i [] [ text subject ]
    , span [] [ text " who agreed with " ]
    , i [] [ text question ]
    ]


{-| Given the text of a survey question, this makes a caption for the chart
in the form:

    Percentage of students who agreed with <survey question text>.

This is used for charts where the x-axis is the score for a particular survey
question across a whole university, and the y-axis the names of the
universities.

-}
percentWhoAgreed : String -> List (Html Msg)
percentWhoAgreed question =
    [ span [] [ text "Percentage of students who agreed with " ]
    , i [] [ text question ]
    ]


{-| It plots one row of data on the chart. The three values it needs are
the ends of the 95% confidence interval and the score. The score is plotted
as a round dot, and the 95% confidence interval is plotted as a horizontal
line passing through the dot.

The arguments are:

    + yoffset: This is a constant for all the y-values on a chart.  It is the
      gap at the top that allows for a caption and a bit of wiggle-room.

    + rowNum: This is 0 for the top row in the chart, 1 for the next row, etc.

    + minConf: The lower end of the 95% confidence interval.

    + val: The score.

    + maxConf: The upper end of the 95% confidence interval.

The values 'minConf', 'val' and 'maxConf' are integers between 0 and 100
inclusive. These are taken directly from the spreadsheet provided by the
Office for Students.

-}
plotRow : Int -> Int -> ( Int, Int, Int ) -> List (Svg.Svg Msg)
plotRow yoffset rowNum ( minConf, val, maxConf ) =
    [ plotDot val rowNum yoffset
    , plotLine minConf maxConf rowNum yoffset
    ]


{-| It makes the line that represents the 95% confidence interval for each
row on the chart.
-}
plotLine : Int -> Int -> Int -> Int -> Svg.Svg Msg
plotLine x1 x2 y yoffset =
    let
        ycoord =
            String.fromInt <| round scale * (y + 1) + yoffset + 1
    in
    Svg.line
        [ Svg.Attributes.x1 <| String.fromInt <| (x1 * plotScale) + gapOnLeft
        , Svg.Attributes.y1 ycoord
        , Svg.Attributes.x2 <| String.fromInt <| (x2 * plotScale) + gapOnLeft
        , Svg.Attributes.y2 ycoord
        , Svg.Attributes.class "errorBar"
        ]
        []


{-| It creates a single dot on the chart.
-}
plotDot : Int -> Int -> Int -> Svg.Svg Msg
plotDot x y yoffset =
    let
        xCoord =
            plotScale * x + gapOnLeft

        yCoord =
            round scale * (y + 1) + yoffset + 1
    in
    circle
        [ cx <| String.fromInt xCoord
        , cy <| String.fromInt yCoord
        , r <| String.fromInt 3
        , fill "black"
        ]
        []


{-| It calculates the list of labels for an axis of the chart. This is
used for calculating the width of the widest label, so the width of the
box containing the chart can be calculated.
-}
labelList : Model -> AxisType -> List String
labelList model axisType =
    maybe2list <| (\( a, b ) -> getCurrentLabels a b) (axisType2Codes axisType model)


{-| It uses the lookup table of code numbers to find the list of string
names for the axis labels.
-}
getCurrentLabels : Dict Int String -> List Int -> Maybe (List String)
getCurrentLabels codes selections =
    combine <| map (\a -> get a codes) selections


{-| The Nothing state is represented as an empty list.
-}
maybe2list : Maybe (List a) -> List a
maybe2list maybeList =
    case maybeList of
        Nothing ->
            []

        Just a ->
            a


{-| It works out the codes and selection list from the axis type.
-}
axisType2Codes : AxisType -> Model -> ( Dict Int String, List Int )
axisType2Codes axisType { unis, questions, chartMode } =
    case axisType of
        UniAxis ->
            case chartMode of
                Overall ->
                    ( overallUniCodes, unis )

                BySubject _ ->
                    ( uniCodes, unis )

        QuestionAxis ->
            ( questionCodes, questions )


{-| The width of a single character in the chart labels. A monospace font is
used, so this will roughly give the amount of space that the label will take
up.
-}
charWidth : Float
charWidth =
    9


{-| It works out the amount of space that needs to be allowed for axis labels.
-}
calcOffset : List String -> Int
calcOffset labels =
    round <| (*) charWidth <| toFloat <| maxStringLength labels


{-| It works out the number of characters in the longest string in a list of
strings.
-}
maxStringLength : List String -> Int
maxStringLength strs =
    case maximum <| map String.length strs of
        Just a ->
            a

        Nothing ->
            0
