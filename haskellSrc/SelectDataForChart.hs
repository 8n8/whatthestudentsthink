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

module SelectDataForChart (nss, nss2) where

{-| It selects the data needed for drawing a chart, given the request
from the front-end.  This is done by mapping over the whole data set
for each request, filtering out the data that are not needed.

This sounds slow, but it was found after a lot of profiling and experimenting
with other methods that this method has pretty good performance.
-}

import qualified Data.IntSet as Si
import Data.List (sortBy)
import qualified General as G
import qualified Parser as P
import Data.Function (on)

{-| Given the data request from outside and the parsed data from file,
this function selects and calculates the data needed for drawing
the chart.  The result is a list of lists, where the inner lists
have three elements each, containing the score and the bounds of the
95% confidence interval.

This is for the NSS2 data set.
-}
{-@ type Int3 = {xs : [Int] | 3 == len xs }@-}
{-@ nss2 :: G.Nss2Input -> [P.IntNss2Line] -> [Int3] @-}
nss2 :: G.Nss2Input -> [P.IntNss2Line] -> [[Int]]
nss2 input = map nss2ToInt . sortNss2ByUni . filter (nss2InOrOut input)

{-| It sorts the data by the integer code of the university. -}
sortNss2ByUni :: [P.IntNss2Line] -> [P.IntNss2Line]
sortNss2ByUni =
  sortBy (compare `on` P.i2Uni)

{-| Given a data point from the NSS2 data set and a request for a new
chart from the frontend, it decides if the data point is needed to
respond to the request.

This function is one of the most used.  In time profiling it was found
that this is the most important function to optimise, since it is called
for every single data point every time there is a request for a new chart
from the frontend.

It was found that using IntSet rather than Set sped things up considerably.
This is one of the main reasons for using integer codes for the universities
and subjects rather than strings.
-}
nss2InOrOut :: G.Nss2Input -> P.IntNss2Line -> Bool
nss2InOrOut input line
  | G.n2Subject input /= P.i2Subject line = False
  | Si.notMember (P.i2Question line) (G.n2Questions input) = False
  | Si.notMember (P.i2Uni line) (G.n2Unis input) = False
  | otherwise = True

{-| It selects the chart data from the whole of the NSS data set,
given the input from the frontend.
-}
{-@ nss :: G.NssInput -> [P.IntNssLine] -> [Int3] @-}
nss :: G.NssInput -> [P.IntNssLine] -> [[Int]]
nss input = map nssToInt . sortNssByUni . filter (nssInOrOut input)

{-| It sorts the data by the university integer code. -}
sortNssByUni :: [P.IntNssLine] -> [P.IntNssLine]
sortNssByUni =
  sortBy (compare `on` P.iUni)

{-| It converts a data point from the NSS2 data set into a list containing
three ints.  These three ints are the bounds of the 95% confidence interval
and the score.
-}
{-@ nss2ToInt :: P.IntNss2Line -> Int3 @-}
nss2ToInt :: P.IntNss2Line -> [Int]
nss2ToInt nss2line =
  [ P.i2MinConf nss2line, P.i2Value nss2line, P.i2MaxConf nss2line ]

{-| It converts a data point from the NSS data set into a list containing
three ints.  See the comment on nss2ToInt.
-}
{-@ nssToInt :: P.IntNssLine -> Int3 @-}
nssToInt :: P.IntNssLine -> [Int]
nssToInt nssLine =
  [ P.iMinConf nssLine, P.iValue nssLine, P.iMaxConf nssLine ]

{-| Given a data point from the NSS data set and a request for a new
chart from the frontend, it decides if the data point is needed to
respond to the request.

Also see the comment on the function 'nss2InOrOut'.
-}
nssInOrOut :: G.NssInput -> P.IntNssLine -> Bool
nssInOrOut input line
  | Si.notMember (P.iqNum line) (G.nQuestions input) = False
  | Si.notMember (P.iUni line) (G.nUnis input) = False
  | otherwise = True
