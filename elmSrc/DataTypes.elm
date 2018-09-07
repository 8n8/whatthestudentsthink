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


module DataTypes
    exposing
        ( AxesConfigType(..)
        , AxisType(..)
        , ChartMode(..)
        , DataChooserMode(..)
        , GetData
        , Model
        , Msg(..)
        , plotScale
        , questionCodes
        , scale
        , yAxisType
        )

{-| It contains the common functions and data types needed by several
modules.
-}

import Dict exposing (Dict, fromList)
import ElmTextSearch exposing (Index)
import Http
import Maybe


axisLabelFontSizeStr : String
axisLabelFontSizeStr =
    toString axisLabelFontSize ++ "%"


axisLabelFontSize : Float
axisLabelFontSize =
    3.7 * scale


{-| The scale factor for the chart.
-}
scale : Float
scale =
    23


plotScale : Int
plotScale =
    3


yAxisType : AxesConfigType -> AxisType
yAxisType axesConfig =
    case axesConfig of
        UniVsA ->
            UniAxis

        QVsA ->
            QuestionAxis


{-| This is for the state of the y-axis. It can either be a list of
universities or a list of survey questions.
-}
type AxisType
    = UniAxis
    | QuestionAxis


{-| There are actually two separate sets of data used to make the charts,
corresponding to the two worksheets 'nss' and 'nss2' in the data spreadsheet
provided by the Office for Students. One gives results for each survey question
across all subjects at each university, and the other gives results for each
subject area for each university. The 'Maybe Int' represents the integer
code of the subject chosen. The 'Nothing' state is for when the user has not
yet chosen a subject.
-}
type ChartMode
    = Overall
    | BySubject (Maybe Int)


type alias Model =
    { data : List ( Int, Int, Int )
    , getRequestErr : Maybe String
    , pageLoading : Bool
    , axesConfig : AxesConfigType
    , chartMode : ChartMode
    , unis : List Int
    , subjectSearchIndex : Index { name : String }
    , subjectSearchResults : List String
    , uniSearchIndex : Index { name : String }
    , uniSearchResults : List String
    , questions : List Int
    , chooserMode : DataChooserMode
    , fatalErr : Maybe String
    }


{-| These are the four possible states of the buttons and checkboxes at the
top for choosing the sort of chart to display. The strings in 'Subject' and
'Uni' are the contents of the search boxes. It is only possible to have one
menu extended at a time.
-}
type DataChooserMode
    = Question
    | Subject String
    | Uni String
    | FoldedUp


type Msg
    = AxesConfigChoice AxesConfigType
    | OverallMode
    | SubjectMode
    | DataRequest (Result Http.Error GetData)
    | UniChooserSearchText String
    | SubjectChooserSearchText String
    | ChooseAUni
    | ChooseAQuestion
    | ChooseASubject
    | ChangeSubject Int
    | ToggleUni Int
    | ToggleQuestion Int
    | ChangeQuestion Int
    | ChangeUni Int
    | ClearAllUnis
    | ClearAllQuestions
    | SelectAllQuestions
    | SelectAllUnis


{-| It represents the two different types of chart available. They both
have the percentage of students who agreed with the question on the x-axis,
but on the y-axis there can be:

    1. A list of universities.

    2. A list of survey questions.

-}
type AxesConfigType
    = UniVsA
    | QVsA


{-| It contains the response from the server when asked for data.
-}
type alias GetData =
    { error : String
    , result : List (List Int)
    }


{-| The main part of the student survey is these 27 statements. Students
are asked how much they agree with each one. These integer codes are
used for convenience, and to make requests to the server more concise.
-}
questionCodes : Dict Int String
questionCodes =
    Dict.fromList
        [ ( 1, "Staff are good at explaining things." )
        , ( 2, "Staff have made the subject interesting." )
        , ( 3, "The course is intellectually stimulating." )
        , ( 4, "My course has challenged me to achieve my best work." )
        , ( 5, "My course has provided me with opportunities to explore ideas or concepts further." )
        , ( 6, "My course has provided me with opportunities to bring information and ideas together from different topics." )
        , ( 7, "My course has provided me with opportunities to apply what I have learnt." )
        , ( 8, "The criteria used in marking have been clear in advance." )
        , ( 9, "Marking and assessment has been fair." )
        , ( 10, "Feedback on my work has been timely." )
        , ( 11, "I have received helpful comments on my work." )
        , ( 12, "I have been able to contact staff when I needed to." )
        , ( 13, "I have received sufficient advice and guidance in relation to my course." )
        , ( 14, "Good advice was available when I needed to make study choices on my course." )
        , ( 15, "The course is well organised and running smoothly." )
        , ( 16, "The timetable works efficiently for me." )
        , ( 17, "Any changes in the course or teaching have been communicated effectively." )
        , ( 18, "The IT resources and facilities provided have supported my learning well." )
        , ( 19, "The library resources (e.g. books, online services and learning spaces) have supported my learning well." )
        , ( 20, "I have been able to access course-specific resources (e.g. equipment, facilities, software, collections) when I needed to." )
        , ( 21, "I feel part of a community of staff and students." )
        , ( 22, "I have had the right opportunities to work with other students as part of my course." )
        , ( 23, "I have had the right opportunities to provide feedback on my course." )
        , ( 24, "Staff value students' views and opinions about the course." )
        , ( 25, "It is clear how students' feedback on the course has been acted on." )
        , ( 26, "The students' union (association or guild) effectively represents students' academic interests." )
        , ( 27, "Overall, I am satisfied with the quality of the course." )
        ]
