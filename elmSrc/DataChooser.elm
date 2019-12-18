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


module DataChooser exposing (dataChooserButtons)

{-| It provides the buttons and checkboxes for filtering the data in the
the chart.
-}

import Data
    exposing
        ( overallUniCodes
        , subjectCodes
        , subjectsOffered
        , uniCodes
        , unisOffering
        )
import DataTypes
    exposing
        ( AxesConfigType(..)
        , ChartMode(..)
        , DataChooserMode(..)
        , Model
        , Msg(..)
        , questionCodes
        )
import Dict exposing (Dict, filter, get, keys, values)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (checked, class, placeholder, style, type_, value)
import Html.Events exposing (onInput)
import List exposing (concat, isEmpty, map, member)
import Svg.Events exposing (onClick)


{-| Given the model (the global state of the app), it creates a list of
Html elements that make up the buttons, checkboxes and menus for choosing
the type of chart to display.
-}
dataChooserButtons : Model -> List (Html Msg)
dataChooserButtons model =
    dataChooserButtons_ (makeChooserConfig model.chooserMode) model


{-| This is what does the real work in this module. 'ConfigShowing' is
a nice convenient representation of the state of the data chooser. The
arguments 'uni', 'question' and 'subject' are bools that show whether
the menus are folded.
-}
dataChooserButtons_ : ConfigShowing -> Model -> List (Html Msg)
dataChooserButtons_ { uni, question, subject, uniSearchText, subjectSearchText } model =
    concat <|
        [ [ axesConfigMode model.axesConfig model.chartMode ]
        , showChooserButton uni ChooseAUni (uniButtonText model)
        , if uni then
            [ uniChooserButtons
                model.chartMode
                model.axesConfig
                uniSearchText
                model.uniSearchResults
                model.unis
            ]

          else
            []
        , showChooserButton question ChooseAQuestion (qButtonText model)
        , if question then
            [ questionChooserButtons model.axesConfig model.questions ]

          else
            []
        , case model.chartMode of
            BySubject maybeSubject ->
                showChooserButton subject
                    ChooseASubject
                    (let
                        txt =
                            subButtonTxt maybeSubject
                     in
                     ( txt, txt )
                    )

            Overall ->
                []
        , case model.chartMode of
            BySubject maybeSubject ->
                if subject then
                    [ subjectChooserButtons
                        model.axesConfig
                        model.chartMode
                        model.unis
                        subjectSearchText
                        model.subjectSearchResults
                        maybeSubject
                    ]

                else
                    []

            Overall ->
                []
        ]


{-| It works out the text to display in the subject chooser button.
-}
subButtonTxt : Maybe Int -> String
subButtonTxt subject =
    case subject of
        Nothing ->
            "Pick a subject area"

        Just _ ->
            "Change subject area"


{-| It works out the text to display on the button for choosing a
survey question.
-}
qButtonText : Model -> ( String, String )
qButtonText { axesConfig, questions } =
    case ( axesConfig, questions ) of
        ( QVsA, [] ) ->
            ( "Pick a survey question", "Hide question chooser" )

        ( QVsA, _ ) ->
            ( "Add or remove survey questions", "Hide question chooser" )

        ( UniVsA, [] ) ->
            ( "Pick a survey question", "Pick a survey question" )

        ( UniVsA, _ ) ->
            ( "Change survey question", "Hide question chooser" )


{-| It works out the text to display on the button for choosing a
university.
-}
uniButtonText : Model -> ( String, String )
uniButtonText { axesConfig, unis } =
    case ( axesConfig, unis ) of
        ( UniVsA, [] ) ->
            ( "Choose some universities", "Hide university chooser" )

        ( UniVsA, _ ) ->
            ( "Add or remove universities", "Hide university chooser" )

        ( QVsA, [] ) ->
            ( "Pick a university", "Hide university chooser" )

        ( QVsA, _ ) ->
            ( "Change university", "Hide university chooser" )


{-| It creates the two radio boxes and the checkbox at the top.
-}
axesConfigMode : AxesConfigType -> ChartMode -> Html Msg
axesConfigMode alreadyChosen chartMode =
    div [ class "mainChoices" ] <|
        List.concat
            [ map (radioButton alreadyChosen) axisMessagesAndLabels
            , [ checkBox Tick
                    (case chartMode of
                        Overall ->
                            SubjectMode

                        BySubject _ ->
                            OverallMode
                    )
                    "Compare by subject area"
                    (chartMode /= Overall)
                    True
              ]
            ]


{-| These are the labels for the radio boxes for choosing the axis layout
of the chart.
-}
axisMessagesAndLabels : List ( AxesConfigType, String )
axisMessagesAndLabels =
    [ ( UniVsA, "Compare universities" )
    , ( QVsA, "Look at one university" )
    ]


{-| It contains a useful expansion of the 'chooserMode' element of the model.
-}
type alias ConfigShowing =
    { uni : Bool
    , question : Bool
    , subject : Bool
    , uniSearchText : String
    , subjectSearchText : String
    }


{-| The default data chooser configuration is that all the menus are folded up.
-}
defaultConf : ConfigShowing
defaultConf =
    { uni = False
    , question = False
    , subject = False
    , uniSearchText = ""
    , subjectSearchText = ""
    }


{-| It expands the 'chooserMode' element of the model into a more useful
representation.
-}
makeChooserConfig : DataChooserMode -> ConfigShowing
makeChooserConfig chooserMode =
    case chooserMode of
        Uni searchText ->
            { defaultConf | uni = True, uniSearchText = searchText }

        Question ->
            { defaultConf | question = True }

        Subject searchText ->
            { defaultConf | subject = True, subjectSearchText = searchText }

        FoldedUp ->
            defaultConf


{-| This makes the buttons that filter the data.
-}
showChooserButton : Bool -> Msg -> ( String, String ) -> List (Html Msg)
showChooserButton showing msg ( showMsg, hideMsg ) =
    [ button
        [ onClick msg
        , class "defaultbutton"
        ]
        [ span
            [ class "buttonText" ]
            [ text <|
                if showing then
                    hideMsg

                else
                    showMsg
            ]
        ]
    ]


{-| This is the message at the top of the list of university checkboxes
that explains what the red text means.
-}
inRedMsgUni : ChartMode -> List (Html Msg)
inRedMsgUni chartMode =
    case chartMode of
        Overall ->
            []

        BySubject (Just _) ->
            [ div
                [ class "redMsg"
                ]
                [ text <|
                    "No data is available for universities in red "
                        ++ "for the current subject area."
                ]
            ]

        BySubject Nothing ->
            []


{-| This is the message at the top of the list of subject chooser
checkboxes to explain what the red text means.
-}
inRedMsgSubject : List Int -> ChartMode -> List (Html Msg)
inRedMsgSubject unis chartMode =
    case ( chartMode, unis ) of
        ( Overall, _ ) ->
            []

        ( BySubject _, [] ) ->
            []

        ( BySubject _, _ ) ->
            [ div
                [ class "redMsg"
                ]
                [ text <|
                    "No data is available for subjects in red for the "
                        ++ "current university."
                ]
            ]


{-| It finds the list of universities that offer the current subject,
if there is one.
-}
availableUnis : ChartMode -> AxesConfigType -> Maybe (List Int)
availableUnis chartMode axConf =
    case chartMode of
        BySubject (Just subject) ->
            get subject unisOffering

        BySubject Nothing ->
            Just <| keys uniCodes

        Overall ->
            Just <| keys overallUniCodes


{-| It makes the drop-down university selector.
-}
uniChooserButtons : ChartMode -> AxesConfigType -> String -> List String -> List Int -> Html Msg
uniChooserButtons chartMode axConf searchText searchResults unis =
    div [ class "chooserSet" ] <|
        concat <|
            case availableUnis chartMode axConf of
                Just available ->
                    [ [ searchBox searchText
                            "Search for a university"
                            UniChooserSearchText
                      ]
                    , clearAndSelect axConf
                    , inRedMsgUni chartMode
                    , dataChooser
                        available
                        (radioOrTick axConf)
                        unis
                        (foundBySearch chartMode searchResults)
                        (checkBoxMsg axConf)
                    ]

                Nothing ->
                    []


{-| It works out the message to send when a university chooser button is
clicked on.
-}
checkBoxMsg : AxesConfigType -> (Int -> Msg)
checkBoxMsg axConf =
    case axConf of
        UniVsA ->
            ToggleUni

        QVsA ->
            ChangeUni


{-| It decides whether the university selector buttons should be checkboxes
or radio buttons.
-}
radioOrTick : AxesConfigType -> CheckBoxType
radioOrTick axConf =
    case axConf of
        UniVsA ->
            Tick

        QVsA ->
            Radio


{-| It makes the two buttons 'Clear all' and 'Select all' that are at the
top of the university selector drop-down.
-}
clearAndSelect : AxesConfigType -> List (Html Msg)
clearAndSelect axConf =
    case axConf of
        UniVsA ->
            concat
                [ clearAll ClearAllUnis
                , selectAll SelectAllUnis
                ]

        QVsA ->
            []


{-| It gives the search results from the university search box.
-}
foundBySearch : ChartMode -> List String -> Dict Int String
foundBySearch chartMode searchResults =
    let
        codes =
            case chartMode of
                Overall ->
                    overallUniCodes

                BySubject _ ->
                    uniCodes
    in
    case searchResults of
        [] ->
            codes

        _ ->
            filterUnis searchResults codes


{-| These are the buttons that fold out when the question filtering button
is clicked on.
-}
questionChooserButtons : AxesConfigType -> List Int -> Html Msg
questionChooserButtons axConf questions =
    let
        contents =
            case axConf of
                QVsA ->
                    [ clearAll ClearAllQuestions
                    , selectAll SelectAllQuestions
                    , dataChooser (keys questionCodes)
                        Tick
                        questions
                        questionCodes
                        ToggleQuestion
                    ]

                UniVsA ->
                    [ dataChooser (keys questionCodes)
                        Radio
                        questions
                        questionCodes
                        ChangeQuestion
                    ]
    in
    div [ class "chooserSet" ] <| concat contents


{-| These are the buttons that fold out when the subject filtering button
is clicked on.
-}
subjectChooserButtons :
    AxesConfigType
    -> ChartMode
    -> List Int
    -> String
    -> List String
    -> Maybe Int
    -> Html Msg
subjectChooserButtons axConf chartMode unis searchText searchResults subject =
    div [ class "chooserSet" ] <|
        concat
            [ [ searchBox searchText
                    "Search for a subject"
                    SubjectChooserSearchText
              ]
            , case axConf of
                UniVsA ->
                    []

                QVsA ->
                    inRedMsgSubject unis chartMode
            , dataChooser
                (case unis of
                    [ uni ] ->
                        case get uni subjectsOffered of
                            Just subjects ->
                                subjects

                            Nothing ->
                                keys subjectCodes

                    _ ->
                        keys subjectCodes
                )
                Radio
                (case subject of
                    Nothing ->
                        []

                    Just a ->
                        [ a ]
                )
                (case searchResults of
                    [] ->
                        subjectCodes

                    _ ->
                        filterUnis searchResults subjectCodes
                )
                ChangeSubject
            ]


{-| It makes the search box that is shown at the top of the drop-down
menus for choosing universities, subjects, etc.
-}
searchBox : String -> String -> (String -> Msg) -> Html Msg
searchBox searchText defaultText msg =
    input
        [ placeholder defaultText
        , value searchText
        , onInput msg
        , class "searchBox"
        , class "defaultbutton"
        ]
        []


{-| It builds the lists of checkboxes for selecting data to display in
the chart.
-}
dataChooser :
    List Int
    -> CheckBoxType
    -> List Int
    -> Dict Int String
    -> (Int -> Msg)
    -> List (Html Msg)
dataChooser enabled checkBoxType alreadyChosen codes msgFunc =
    map (chooserCheckBox enabled checkBoxType alreadyChosen codes msgFunc)
        (keys codes)


{-| It makes the 'Select All' button at the top of the drop-down menus
under the chooser buttons.
-}
selectAll : Msg -> List (Html Msg)
selectAll msg =
    [ button [ onClick msg, class "defaultbutton" ] [ text "Select all" ]
    ]


{-| Makes the 'Clear All' button at the top of the drop-down menus under
the chooser buttons.
-}
clearAll : Msg -> List (Html Msg)
clearAll msg =
    [ button [ onClick msg, class "defaultbutton" ] [ text "Clear all" ]
    ]


type CheckBoxType
    = Radio
    | Tick


{-| It makes a checkbox, either radio or tick. The arguments are:

    + enabled: A list of the integer codes of the options that are enabled.
      This has a different meaning from usual, in that a disabled checkbox
      still works, and it is coloured red, not grey.  Checkboxes are disabled
      when, for example, a university does not offer a subject.  The reason
      for showing it in its 'disabled' state at all is that someone might
      want to change subject first, then look in the universities list
      and choose a university (not in red) that offers it.

    + checkBoxType: A radio button or a tick-box.

    + alreadyChosen:  The integer codes of the already selected options.

    + codes:  The integer code dictionary from Data.elm for looking up
      the text corresponding to the integer codes.

    + msgFunc: This is the Msg that will be created when the checkbox
      is clicked on.

    + thisChoice: The integer code for this checkbox.

-}
chooserCheckBox :
    List Int
    -> CheckBoxType
    -> List Int
    -> Dict Int String
    -> (Int -> Msg)
    -> Int
    -> Html Msg
chooserCheckBox enabled checkBoxType alreadyChosen codes msgFunc thisChoice =
    let
        ticked =
            member thisChoice alreadyChosen

        thisEnabled =
            List.member thisChoice enabled

        label =
            case get thisChoice codes of
                Nothing ->
                    String.fromInt thisChoice

                Just str ->
                    str
    in
    checkBox checkBoxType (msgFunc thisChoice) label ticked thisEnabled


{-| A lower level function used by 'chooserCheckBox' to make the checkbox.
-}
checkBox : CheckBoxType -> Msg -> String -> Bool -> Bool -> Html Msg
checkBox checkBoxType msg label ticked enabled =
    Html.label
        []
        [ input
            [ type_ <| boxType2String checkBoxType
            , onClick msg
            , checked ticked
            ]
            []
        , div
            [ class
                (if enabled then
                    "blackCheckBox"

                 else
                    "redCheckBox"
                )
            , class "generalCheckBox"
            ]
            [ text label ]
        ]


boxType2String : CheckBoxType -> String
boxType2String boxType =
    case boxType of
        Radio ->
            "radio"

        Tick ->
            "checkbox"


{-| It is used for making the two radio buttons at the top, not the ones
in the chooser menus.
-}
radioButton : AxesConfigType -> ( AxesConfigType, String ) -> Html Msg
radioButton alreadyChosen ( thisOne, label ) =
    checkBox Radio
        (AxesConfigChoice thisOne)
        label
        (alreadyChosen == thisOne)
        True


{-| It is used for reducing the university chooser checkboxes displayed,
depending on the search results from the search box.
-}
filterUnis : List String -> Dict Int String -> Dict Int String
filterUnis results =
    filter (\_ v -> member v results)
