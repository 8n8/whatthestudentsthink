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


module Update exposing (makeSearchIndex, update)

{-| Provides the main 'update' function for the app.
-}

import Data exposing (overallUniCodes, subjectsOffered, uniCodes, unisOffering)
import DataTypes
    exposing
        ( AxesConfigType(..)
        , ChartMode(..)
        , DataChooserMode(..)
        , Model
        , Msg(..)
        , questionCodes
        )
import Dict exposing (Dict, get, keys)
import ElmTextSearch exposing (Index, addDocs, new, search)
import FreshData exposing (freshData)
import List exposing (filter, map, member)
import Maybe.Extra exposing (combine)
import Tuple exposing (first)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- The university chooser button has been clicked on, so it opened if
        -- folded up and vice versa.
        ChooseAUni ->
            ( { model
                | uniSearchResults = []
                , chooserMode =
                    case model.chooserMode of
                        Uni _ ->
                            FoldedUp

                        _ ->
                            Uni ""
              }
            , Cmd.none
            )

        -- The question chooser button has been clicked on, so it is opened
        -- or folded up, depending on its current state.
        ChooseAQuestion ->
            ( { model
                | chooserMode =
                    case model.chooserMode of
                        Question ->
                            FoldedUp

                        _ ->
                            Question
              }
            , Cmd.none
            )

        -- The subject chooser button has been clicked on, so it is opend or
        -- folded up, depending on its current state.
        ChooseASubject ->
            ( { model
                | subjectSearchResults = []
                , chooserMode =
                    case model.chooserMode of
                        Subject _ ->
                            FoldedUp

                        _ ->
                            Subject ""
              }
            , Cmd.none
            )

        -- The radio button for comparing universities has been clicked on.
        AxesConfigChoice UniVsA ->
            freshModel
                { model
                    | axesConfig = UniVsA
                    , unis =
                        case model.chartMode of
                            BySubject (Just subj) ->
                                case get subj unisOffering of
                                    Nothing ->
                                        []

                                    Just unis ->
                                        unis

                            BySubject Nothing ->
                                keys uniCodes

                            Overall ->
                                keys overallUniCodes
                    , questions = [ 27 ]
                    , chooserMode = FoldedUp
                }

        -- The radio button for looking at the results for a single university
        -- have been clicked on.
        AxesConfigChoice QVsA ->
            freshModel
                { model
                    | axesConfig = QVsA
                    , unis = []
                    , questions = keys questionCodes
                    , chooserMode = Uni ""
                }

        -- The 'Select All' button in the university chooser menu has been
        -- clicked on.
        SelectAllUnis ->
            freshModel <|
                case model.chartMode of
                    Overall ->
                        { model | unis = keys overallUniCodes }

                    BySubject (Just subject) ->
                        { model
                            | unis =
                                case get subject unisOffering of
                                    Nothing ->
                                        keys uniCodes

                                    Just unis ->
                                        unis
                        }

                    BySubject Nothing ->
                        { model | unis = keys uniCodes }

        -- The 'Clear All' button in the university chooser menu has been
        -- clicked on.
        ClearAllUnis ->
            freshModel { model | unis = [] }

        -- The 'Select All' button in the subject chooser menu has been
        -- clicked on.
        SelectAllQuestions ->
            freshModel { model | questions = keys questionCodes }

        -- The 'Clear All' button in the question chooser menu has been
        -- clicked on.
        ClearAllQuestions ->
            freshModel { model | questions = [] }

        -- Something has been typed in the search box in the university
        -- chooser menu.
        UniChooserSearchText txt ->
            let
                ( searchResults, newSearchIndex ) =
                    search4words model.uniSearchIndex txt
            in
            ( { model
                | chooserMode = Uni txt
                , uniSearchIndex = newSearchIndex
                , uniSearchResults = searchResults
              }
            , Cmd.none
            )

        -- Something has been typed in the search box in the subject
        -- chooser menu.
        SubjectChooserSearchText txt ->
            let
                ( searchResults, newSearchIndex ) =
                    search4words model.subjectSearchIndex txt
            in
            ( { model
                | chooserMode = Subject txt
                , subjectSearchIndex = newSearchIndex
                , subjectSearchResults = searchResults
              }
            , Cmd.none
            )

        -- One of the unticked radio buttons in the subject chooser menu has been
        -- clicked on.
        ChangeSubject subject ->
            case ( get subject unisOffering, model.axesConfig ) of
                ( Nothing, _ ) ->
                    ( { model
                        | fatalErr =
                            Just <|
                                "Could not find subject in 'unisOffering'"
                                    ++ " dictionary."
                      }
                    , Cmd.none
                    )

                ( Just unis, UniVsA ) ->
                    freshModel
                        { model
                            | chartMode = BySubject (Just subject)
                            , unis =
                                case model.unis of
                                    [ _ ] ->
                                        model.unis

                                    _ ->
                                        unis
                            , chooserMode = FoldedUp
                        }

                ( Just _, QVsA ) ->
                    freshModel
                        { model
                            | chartMode = BySubject (Just subject)
                            , uniSearchResults = []
                            , chooserMode =
                                case model.unis of
                                    [] ->
                                        Uni ""

                                    _ ->
                                        FoldedUp
                        }

        -- One of the unselected radio buttons in the university chooser menu
        -- has been clicked on.  This message only happens when the chart is
        -- in the mode for looking at each question for a single university.
        ChangeUni uni ->
            case get uni subjectsOffered of
                Nothing ->
                    ( { model
                        | fatalErr =
                            Just <|
                                "Could not find university in 'subjectsOffered'"
                                    ++ " dictionary."
                      }
                    , Cmd.none
                    )

                Just subjects ->
                    freshModel
                        { model
                            | unis = [ uni ]
                            , subjectSearchResults = []
                            , chooserMode =
                                case model.chartMode of
                                    Overall ->
                                        FoldedUp

                                    BySubject Nothing ->
                                        Subject ""

                                    BySubject (Just _) ->
                                        FoldedUp
                        }

        -- One of the tick-boxes in the question chooser menu has been
        -- clicked on, either selecting or unselecting it.  This message
        -- only happens when the chart is in the mode where it contains data
        -- for all the questions at a single university.
        ToggleQuestion question ->
            freshModel
                { model
                    | questions =
                        calculateDataChoice model.questions question
                }

        -- One of the radio buttons in the question chooser menu has been
        -- clicked on.
        ChangeQuestion q ->
            freshModel
                { model
                    | questions = [ q ]
                    , chooserMode =
                        case model.chartMode of
                            Overall ->
                                FoldedUp

                            BySubject Nothing ->
                                Subject ""

                            BySubject (Just _) ->
                                FoldedUp
                }

        -- One of the tick-boxes in the university chooser menu has been
        -- toggled.
        ToggleUni uni ->
            freshModel
                { model | unis = calculateDataChoice model.unis uni }

        -- The 'compare by subject' tick-box has been unselected.
        OverallMode ->
            freshModel <|
                case model.axesConfig of
                    UniVsA ->
                        { model
                            | chartMode = Overall
                            , unis = keys overallUniCodes
                            , questions = [ 27 ]
                            , chooserMode = FoldedUp
                            , uniSearchIndex = makeSearchIndex overallUniCodes
                        }

                    QVsA ->
                        { model
                            | chartMode = Overall
                            , unis = []
                            , questions = keys questionCodes
                            , chooserMode = Uni ""
                            , uniSearchIndex = makeSearchIndex overallUniCodes
                        }

        -- The 'compare by subject' tick-box has been selected.
        SubjectMode ->
            freshModel
                { model
                    | chartMode = BySubject Nothing
                    , chooserMode = Subject ""
                    , unis = []
                    , uniSearchIndex = makeSearchIndex uniCodes
                }

        -- A JSON response has been sent back from the server and decoded.
        DataRequest dat ->
            case dat of
                Err err ->
                    ( { model
                        | getRequestErr = Just (toString err)
                        , pageLoading = False
                      }
                    , Cmd.none
                    )

                Ok { error, result } ->
                    if error == "" then
                        case data2Tuple result of
                            Just goodresult ->
                                ( { model
                                    | data = goodresult
                                    , getRequestErr = Nothing
                                    , pageLoading = False
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { model
                                    | data = []
                                    , getRequestErr =
                                        Just "Couldn't convert data to tuples."
                                    , pageLoading = False
                                  }
                                , Cmd.none
                                )
                    else
                        ( { model
                            | getRequestErr = Just error
                            , pageLoading = False
                          }
                        , Cmd.none
                        )


{-| The data from the server is a list of lists of ints, where the inner
lists each contain exactly three elements. This inner list is better
represented by a tuple, since then the type system will guarantee that it
is always the right size.
-}
data2Tuple : List (List Int) -> Maybe (List ( Int, Int, Int ))
data2Tuple dat =
    combine <| map oneDat2Tuple dat


{-| Converts a list containing three elements to a three-tuple.
-}
oneDat2Tuple : List Int -> Maybe ( Int, Int, Int )
oneDat2Tuple dat =
    case dat of
        [ a, b, c ] ->
            Just ( a, b, c )

        _ ->
            Nothing


{-| It makes a request for fresh data from the server and updates the model.
-}
freshModel : Model -> ( Model, Cmd Msg )
freshModel ({ questions, unis } as model) =
    if List.any List.isEmpty [ questions, unis ] then
        ( model, Cmd.none )
    else
        ( { model
            | pageLoading = True
          }
        , freshData model
        )


{-| It is used to update the lists of selected data when the checkboxes
are clicked on in the data selector.
-}
calculateDataChoice : List Int -> Int -> List Int
calculateDataChoice currentChoices new =
    if List.member new currentChoices then
        List.filter (\x -> x /= new) currentChoices
    else
        List.sort <| new :: currentChoices


{-| This powers the search boxes. It takes in the search index and the text
from the search box, and returns the list of search results and the index.
-}
search4words :
    Index { name : String }
    -> String
    -> ( List String, Index { name : String } )
search4words index searchString =
    case search searchString index of
        Err msg ->
            ( [], index )

        Ok ( newIndex, resultList ) ->
            ( map first resultList, newIndex )


{-| It creates the search index. This is used whenever the data changes.
It is faster to reuse the index where possible, rather than regenerating
it for each search. The university and subject search indexes are stored
in the model.
-}
makeSearchIndex : Dict Int String -> Index { name : String }
makeSearchIndex codes =
    first <| addDocs (indexContent codes) initIndex


{-| An empty index to update. Required when making a new one.
-}
initIndex : Index { name : String }
initIndex =
    new
        { ref = .name
        , fields = [ ( .name, 1.0 ) ]
        , listFields = []
        }


{-| Extracts the keys from the dictionary and saves each one in a record
ready to be indexed.
-}
indexContent : Dict Int String -> List { name : String }
indexContent codes =
    map (\x -> { name = x }) (Dict.values codes)
