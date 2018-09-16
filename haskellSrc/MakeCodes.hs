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

module MakeCodes
  ( nss
  , nss2
  ) where

import           Data.List  (nub, sort)
{-| It makes integer codes for the universities for both datasets. -}
import qualified Data.Map   as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Text  as T
import qualified General    as G
import qualified Parser     as P

{-| It extracts the university names from the 'NSS' worksheet in the
data spreadsheet, which is the one containing the overall university
scores.  It sorts the names in alphabetical order, removes duplicates,
and numbers them, starting at 1.
-}
nss :: [P.NssLine] -> G.NssCodes
nss dat =
  let uniNames = sort . nub $ map P.nUniName dat
   in G.NssCodes $ zip uniNames [1 ..]

{-| It extracts the university names and subject names from the detailed
data worksheet 'NSS2'.  This is the one that gives scores for each university
for each subject area.  It also compiles a list of subjects offered at each
university, and a list of universities that offer it for each subject.
-}
nss2 :: [P.Nss2Line] -> G.Nss2Codes
nss2 dat =
  let unis = zip (sort . nub $ map P.n2UniName dat) [1 ..]
      subjects = zip (sort . nub $ map P.n2Subject dat) [1 ..]
   in G.Nss2Codes
        { G.c2Unis = unis
        , G.c2Subjects = subjects
        , G.c2SubjectsOffered = subjectsOffered dat unis subjects
        , G.c2UnisOffering = unisOffering dat unis subjects
        }

{-| It calculates which universities offer each subject.
-}
unisOffering ::
     [P.Nss2Line] -> [(T.Text, Int)] -> [(T.Text, Int)] -> [(Int, [Int])]
unisOffering points unisl subjectsl =
  let names :: [T.Text]
      names = map fst subjectsl
      codes :: [Int]
      codes = map snd subjectsl
      uniNames :: [[T.Text]]
      uniNames = map (subjectUnis points) names
      uniCodes :: [[Int]]
      uniCodes = map (convertToInts unisl) uniNames
   in zip codes uniCodes

{-| It makes a list of the universities that offer a particular subject.
-}
subjectUnis :: [P.Nss2Line] -> T.Text -> [T.Text]
subjectUnis points subject =
  sort . nub . map P.n2UniName . filter ((== subject) . P.n2Subject) $ points

{-| It makes a list of the subjects offered for each university.
-}
subjectsOffered ::
     [P.Nss2Line] -> [(T.Text, Int)] -> [(T.Text, Int)] -> [(Int, [Int])]
subjectsOffered points unisl subjectsl =
  let names :: [T.Text]
      names = map fst unisl
      codes :: [Int]
      codes = map snd unisl
      subNames :: [[T.Text]]
      subNames = map (uniSubjects points) names
      subCodes :: [[Int]]
      subCodes = map (convertToInts subjectsl) subNames
   in zip codes subCodes

{-| It makes a list of the subjects offered by a particular university.
-}
uniSubjects :: [P.Nss2Line] -> T.Text -> [T.Text]
uniSubjects points uniName =
  sort . nub . map P.n2Subject . filter ((== uniName) . P.n2UniName) $ points

{-| It converts text strings to ints using a lookup table.  This is used for
converting lists of subjects and universities to their corresponding codes.
-}
convertToInts :: [(T.Text, Int)] -> [T.Text] -> [Int]
convertToInts codeMap names =
  sort . mapMaybe (`Map.lookup` Map.fromList codeMap) $ names
