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


module AxisLabels exposing (gapOnLeft, xAxisLabels, yAxisLabels)

{-| It makes the labels for the x and y axes.
-}

import Data exposing (overallUniCodes, uniCodes)
import DataTypes
    exposing
        ( AxisType(..)
        , ChartMode(..)
        , Model
        , Msg(..)
        , plotScale
        , questionCodes
        , scale
        , yAxisType
        )
import Dict exposing (get, keys)
import List exposing (length, map, map2, range)
import Maybe.Extra exposing (combine)
import Svg exposing (text, text_)
import Svg.Attributes exposing (class, fill, textAnchor, transform, x, y)


{-| Makes the labels for the y-axis. These are either university
names or survey questions.
-}
yAxisLabels : Model -> Int -> Int -> Maybe (List (Svg.Svg Msg))
yAxisLabels model xoffset yoffset =
    let
        axisType =
            yAxisType model.axesConfig

        yVals =
            axisValues model axisType

        yPositions =
            range 1 (length yVals)
    in
    combine <|
        map2
            (oneYLabel model.chartMode axisType yoffset)
            yPositions
            yVals


{-| Makes the labels for the x-axis. These are the percentages of
students who agreed with a survey question.
-}
xAxisLabels : Model -> Int -> Int -> Int -> List (Svg.Svg Msg)
xAxisLabels model xoffset yoffset numYLabels =
    let
        xPositions =
            [ 0, 20, 40, 60, 80, 100 ]

        grid =
            verticalLines yoffset numYLabels
    in
    grid Thick [ 0, 20, 40, 60, 80, 100 ]
        ++ grid Medium [ 10, 30, 50, 70, 90 ]
        ++ grid Thin [ 5, 15, 25, 35, 45, 55, 65, 75, 85, 95 ]
        ++ map (oneXLabel yoffset) xPositions


{-| These are the thicknesses of the vertical lines on the chart. There
are thick lines to mark multiples of 20%, medium lines to mark multiples of
10%, and thin lines for the percentages ending in 5. The actual weights
are defined in CSS.
-}
type LineWeight
    = Thin
    | Medium
    | Thick


{-| It makes the vertical lines that mark the percentages in the chart. See
the comment by the LineWeight type for more detail.
-}
verticalLines : Int -> Int -> LineWeight -> List Int -> List (Svg.Svg Msg)
verticalLines yoffset maxY weight xpositions =
    map (oneVerticalLine weight yoffset maxY) xpositions


{-| This is a small margin on the left-hand side of the chart. It is
necessary because the left-hand zero mark on the chart needs to be slightly
indented so that the zero axis label and the zero line are not cut in half
by the edge of the SVG area.
-}
gapOnLeft : Int
gapOnLeft =
    plotScale * 2


{-| It makes one of the vertical marker lines on the chart. There is one
every 5% on the horizontal axis, with thick lines at 0%, 20% etc, medium
lines at 10%, 30% etc, and thin lines at 5%, 15% etc.
-}
oneVerticalLine : LineWeight -> Int -> Int -> Int -> Svg.Svg Msg
oneVerticalLine weight yoffset maxY xPos =
    let
        xCoord =
            toString <| xPos * plotScale + gapOnLeft
    in
    Svg.line
        [ Svg.Attributes.x1 xCoord
        , Svg.Attributes.x2 xCoord
        , Svg.Attributes.y1 <| toString <| yoffset + (round <| scale * 0.5)
        , Svg.Attributes.y2 <| toString <| yoffset + round scale * maxY + 15
        , class
            (case weight of
                Thick ->
                    "thickline"

                Medium ->
                    "mediumline"

                Thin ->
                    "thinline"
            )
        ]
        []


{-| It makes one of the y-labels. The arguments are:

    + chartMode: This says whether the chart is organised by subject or
      just has overall scores for each university across all subjects.

    + axisType: This says whether the y-axis is university names or survey
      questions.

    + yoffset: This is the constant amount by which all y-labels are shifted
      down from the top of the SVG area, to allow room for the title etc.

    + pos: This is different for each y-label in a chart, and says how far
      down the chart the label is from the top.

    + labelID: This is an integer code for looking up the text to display.
      It could be either a university code or a survey question number.  This
      is the reason for the chartMode and axisType arguments, so the it knows
      where to look for the text label.

-}
oneYLabel : ChartMode -> AxisType -> Int -> Int -> Int -> Maybe (Svg.Svg Msg)
oneYLabel chartMode axisType yoffset pos labelID =
    let
        ypos =
            pos * round scale + yoffset + 5
    in
    case getLabel chartMode axisType labelID of
        Nothing ->
            Nothing

        Just label ->
            Just <|
                text_
                    [ x <| toString <| 100 * plotScale + gapOnLeft + 12
                    , y <| toString ypos
                    , textAnchor "left"
                    , fill "black"
                    ]
                    [ Svg.text label
                    ]


{-| It makes one of the labels on the x-axis. The 'yoffset' argument is the
constant offset down from the top of the SVG box that contains the chart. This
allows room for stuff at the top, like the title. The 'pos' argument is the
horizontal position on the chart to put the label, like 20 for the 20% label.
-}
oneXLabel : Int -> Int -> Svg.Svg Msg
oneXLabel yoffset pos =
    let
        xpos =
            pos * plotScale + gapOnLeft
    in
    text_
        [ x <| toString xpos
        , y <| toString <| yoffset + 2
        , textAnchor "middle"
        , fill "black"
        ]
        [ text <| toString pos
        ]


{-| It is for looking up the text to display in a y-axis label. The
university names and question texts are stored in Elm dictionaries, with
int keys and string values. These dictionaries are generated code, made
by the Haskell 'generateCodes' program. They are put in the file Data.elm.

The Office for Students provides separate spreadsheet worksheets for the
overall scores and for the by-subject scores. There are different universities
in each of the worksheets because of the publication thresholds. So, for
example, there might be a high enough response from the students studying
a particular subject at a university, but not high enough response rate for
the university overall.

The 'uniCodes' dictionary contains the university codes for the universities
that are in the by-subject spreadsheet worksheet (called 'nss2') and the
'overallUniCodes' dictionary contains the codes for the universities in
the overall scores spreadsheet worksheet (called 'nss').

-}
getLabel : ChartMode -> AxisType -> Int -> Maybe String
getLabel chartMode axisType code =
    case ( chartMode, axisType ) of
        ( BySubject _, UniAxis ) ->
            get code uniCodes

        ( Overall, UniAxis ) ->
            get code overallUniCodes

        ( _, QuestionAxis ) ->
            get code questionCodes


{-| It gets the correct set of integer codes for the labels of an axis. The
model (the global state of the app), in its 'unis' and 'questions' fields, holds
the integer codes of the currently selected universities and questions.

For convenience, for both the 'unis' and 'questions' fields in the model, the
empty list is used to mean 'all'. So when the list is empty, this function
returns all the codes, taken from the keys of the appropriate int code
dictionary.

-}
axisValues : Model -> AxisType -> List Int
axisValues model axisType =
    case axisType of
        UniAxis ->
            case ( model.unis, model.chartMode ) of
                ( [], BySubject _ ) ->
                    keys uniCodes

                ( [], Overall ) ->
                    keys overallUniCodes

                ( unis, _ ) ->
                    unis

        QuestionAxis ->
            case model.questions of
                [] ->
                    keys questionCodes

                questions ->
                    questions
