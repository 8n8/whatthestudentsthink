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
{-# LANGUAGE OverloadedStrings #-}

module MakeElmCode
  ( elmify
  ) where

import qualified Data.Text as T
{-| It generates the Elm code.  This is so that the backend and frontend
use the same integer codes for subjects and universities.  The results
eventually get put in the file Data.elm in the elmSrc directory.  It makes
five Elm dictionaries:

    1. uniCodes: Integer codes for the universities in the detailed
       by-subject 'nss2' worksheet.

    2. subjectCodes: Integer codes for the subjects.

    3. subjectsOffered: The subjects offered at each university.

    4. unisOffereng: The universities that offer each subject.

    5. overallUniCodes: Integer codes for the universities in the
       overall 'nss' worksheet.

-}
import qualified General   as G

{-| It converts the lookup tables of names to numbers to Elm code, for
both the universities and the subject areas.
-}
elmify :: G.Nss2Codes -> G.NssCodes -> T.Text
elmify nss2Codes (G.NssCodes nssCodes) =
  T.concat
    [ preamble
    , "\n"
    , elmifyCode "uniCodes" $ reverseCode $ G.c2Unis nss2Codes
    , "\n"
    , elmifyCode "subjectCodes" $ reverseCode $ G.c2Subjects nss2Codes
    , "\n"
    , elmifyOffered "subjectsOffered" $ G.c2SubjectsOffered nss2Codes
    , "\n"
    , elmifyOffered "unisOffering" $ G.c2UnisOffering nss2Codes
    , "\n"
    , elmifyCode "overallUniCodes" $ reverseCode nssCodes
    ]

{-| The declaration of the dictionaries that have type "Dict Int (List Int)".
These are for dictionaries like the lists of subjects offered at each
university.  In this case the keys are university codes and the lists are
the integer codes of the subjects at that university.
-}
offeredPreamble :: T.Text -> T.Text
offeredPreamble name =
  T.concat [name, " : Dict Int (List Int)\n", name, " = fromList\n"]

{-| It makes the dictionaries that have type "Dict Int (List Int)". -}
elmifyOffered :: T.Text -> [(Int, [Int])] -> T.Text
elmifyOffered name [] = T.concat [offeredPreamble name, "  [ ]"]
elmifyOffered name subs =
  T.concat
    [ offeredPreamble name
    , elmifyOneOffered '[' (head subs)
    , T.concat $ map (elmifyOneOffered ',') (tail subs)
    , "  ]\n"
    ]

{-| It makes one line in a dictionary with type "Dict Int (List Int)".
It might be something like this:

    , (8, [ 30, 41 ])

-}
elmifyOneOffered :: Char -> (Int, [Int]) -> T.Text
elmifyOneOffered startChar code =
  T.concat ["  ", T.singleton startChar, " ", showOffered code, "\n"]

{-| It makes part of a line in a dictionary with type "Dict Int (List Int)".
Like this:

    (17, [ 3, 11, 19, 29 ])
-}
showOffered :: (Int, [Int]) -> T.Text
showOffered (uniNum, subList) =
  T.concat ["(", T.pack . show $ uniNum, ", ", showIntList subList, ")"]

{-| It makes a list of Ints in Elm. -}
showIntList :: [Int] -> T.Text
showIntList [] = "[ ]"
showIntList ints =
  T.concat
    [showInt '[' (head ints), T.concat $ map (showInt ',') (tail ints), " ]"]

{-| It makes an Elm representation of an Int, preceded by a character.  This
is actually only used for lists, so the character would be a comma.
-}
showInt :: Char -> Int -> T.Text
showInt startChar int =
  T.concat [T.singleton startChar, " ", T.pack . show $ int]

{-| This is for making the Elm dictionarys that map Int to String,
like mapping an integer code to the name of a university.
-}
elmifyCode :: T.Text -> [(Int, T.Text)] -> T.Text
elmifyCode name [] = T.concat [codePreamble name, "[]\n"]
elmifyCode name codes =
  T.concat
    [ codePreamble name
    , elmifyOneCode '[' (head codes)
    , T.concat $ map (elmifyOneCode ',') (tail codes)
    , "  ]\n"
    ]

{-| The declaration part of the dictionaries that map Int to String. -}
codePreamble :: T.Text -> T.Text
codePreamble name =
  T.concat [name, " : Dict Int String\n", name, " = fromList\n"]

{-| It converts a tuple to a piece of elm code.  The 'startChar'
variable should be either a comma or a '['.
-}
elmifyOneCode :: Char -> (Int, T.Text) -> T.Text
elmifyOneCode startChar code =
  T.concat ["  ", T.singleton startChar, " ", showCode code, "\n"]

{-| It creates part of a line of the type of dictionary that map Int to
String.  So the output might be:

    (35, "Psycology")

-}
showCode :: (Int, T.Text) -> T.Text
showCode (num, str) = T.concat ["(", T.pack . show $ num, ", \"", str, "\")"]

{-| The preamble of the whole Data module. -}
preamble :: T.Text
preamble =
  T.concat
    [ "module Data exposing\n"
    , "    ( uniCodes\n"
    , "    , subjectCodes\n"
    , "    , subjectsOffered\n"
    , "    , unisOffering\n"
    , "    , overallUniCodes\n"
    , "    )\n"
    , "import Dict exposing (Dict, fromList)\n"
    ]

reverseCode :: [(T.Text, Int)] -> [(Int, T.Text)]
reverseCode = map reverse2tup

reverse2tup :: (a, b) -> (b, a)
reverse2tup (a, b) = (b, a)
