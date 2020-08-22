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

module MakeElm (makeElm) where

{-| It converts the raw data files into Elm Code. -}

import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8')
import qualified Parser as P
import qualified Data.Set as S
import qualified Data.Text as T
import qualified MakeCodes
import Data.List (nub)
import qualified Data.Map as Map
import qualified General as G
import GenerateElm (elmify)


{-| It reads in the two CSV data files, parses them, removes the
small universities and creates the integer codes.
-}
makeElm :: B.ByteString -> B.ByteString -> (Either String T.Text)
makeElm nssRaw nss3Raw =
    case (decodeUtf8' nssRaw, decodeUtf8' nss3Raw) of
        (Left err, _) ->
            Left $ "decoding UTF8 in nss: " ++ show err

        (_, Left err) ->
            Left $ "decoding UTF8 in nss3: " ++ show err

        (Right nssText, Right nss3Text) ->
            case (P.nss nssText, P.nss3 nss3Text) of
                (Left err, _) ->
                    Left $ "parsing nss: " ++ show err

                (_, Left err) ->
                    Left $ "parsing nss3: " ++ show err

                (Right nss, Right nss3) ->
                    processParsed nss nss3


{-| It removes small universities, replaces the text of the
university names with integer codes, and generates the Elm code.
-}
processParsed :: [P.NssLine] -> [P.Nss3Line] -> Either String T.Text
processParsed nss nss3 =
    let
        bigNssUnis = findBigNssUnis (S.map fst bigNss3Unis) nss
        bigNss3Unis = findBigNss3Unis nss3
        wordyNss = removeTinyUnis bigNssUnis nss
        wordyNss3 = removeTinyUnis3 bigNss3Unis nss3
        nssCodes = MakeCodes.nss wordyNss
        nss3Codes = MakeCodes.nss3 wordyNss3
        maybeNssInt = nssToInt nssCodes wordyNss
        maybeNss3Int = nss3ToInt nss3Codes wordyNss3
    in case (maybeNss3Int, maybeNssInt) of
        (Just nss3Int, Just nssInt) ->
            Right $ elmify (nssInt, nss3Int, nssCodes, nss3Codes)

        _ ->
            Left "could not convert universities and subjects to ints"


{-| Given a list of Maybes, it returns Nothing if any of them is
Nothing, and the values if they are all Just.
-}
fromMaybes :: [Maybe a] -> Maybe [a]
fromMaybes maybes =
    fromMaybesHelp maybes []


fromMaybesHelp :: [Maybe a] -> [a] -> Maybe [a]
fromMaybesHelp maybes accum =
    case maybes of
        [] ->
            Just $ reverse accum

        Nothing : _ ->
            Nothing

        Just m : aybes ->
            fromMaybesHelp aybes (m : accum)


{-| It takes in the parsed data from the 'NSS' worksheet and the
integer codes for the universities, and produces a dataset with the
string university names replaced by their integer codes.
-}
nssToInt :: G.NssCodes -> [P.NssLine] -> Maybe [P.IntNssLine]
nssToInt codes dat =
    let
        uniMap = Map.fromList $ G.cUnis codes
    in
        fromMaybes . map (oneNssToInt uniMap) $ dat


{-| It takes in a map of university names to their integer codes, and
a single line from the 'NSS' worksheet.  The output is the data from
the line, but with the university name replaced with its integer code.
-}
oneNssToInt :: Map.Map T.Text Int -> P.NssLine -> Maybe P.IntNssLine
oneNssToInt uniMap line =
    case Map.lookup (P.nUniName line) uniMap of
        Just uni ->
            Just P.IntNssLine
                { P.iUni = uni
                , P.iqNum = P.nQNum line
                , P.iMinConf = P.nMinConf line
                , P.iValue = P.nValue line
                , P.iMaxConf = P.nMaxConf line
                , P.iSampleSize = P.nSampleSize line
                }

        Nothing ->
            Nothing


{-| It takes in the integer codes for universities and subjects for
the 'NSS3' worksheet, and the parsed data.  Its output is the same
data but with the subject and university names replaced with their
integer codes.
-}
nss3ToInt :: G.Nss3Codes -> [P.Nss3Line] -> Maybe [P.IntNss3Line]
nss3ToInt codes dat =
    let
        uniMap = Map.fromList $ G.c3Unis codes
        subjMap = Map.fromList $ G.c3Subjects codes
    in
        fromMaybes . map (oneNss3ToInt uniMap subjMap) $ dat


{-| It converts the university and subject names in one line from
the 'NSS3' worksheet into their integer codes.
-}
oneNss3ToInt
    :: Map.Map T.Text Int
    -> Map.Map T.Text Int
    -> P.Nss3Line
    -> Maybe P.IntNss3Line
oneNss3ToInt uniMap subjMap line =
    case ( Map.lookup (P.n3UniName line) uniMap
         , Map.lookup (P.n3Subject line) subjMap) of
        ( Just uni, Just subj ) ->
            Just
                P.IntNss3Line
                    { P.i3Uni = uni
                    , P.i3Subject = subj
                    , P.i3Question = P.n3Question line
                    , P.i3MinConf = P.n3MinConf line
                    , P.i3Value = P.n3Value line
                    , P.i3MaxConf = P.n3MaxConf line
                    , P.i3SampleSize = P.n3SampleSize line
                    }

        _ ->
            Nothing
  

{-| It takes in a list of all the larger university names and the
whole of the parsed data from the 'NSS' worksheet, and removes all
the small universities from it. This was done because it was found
that the charts were cluttered up with lots of very small
universities.
-}
removeTinyUnis :: S.Set T.Text -> [P.NssLine] -> [P.NssLine]
removeTinyUnis bigUnis =
    filter (flip S.member bigUnis . P.nUniName)


{-| Removes small universities from the 'NSS3' data. -}
removeTinyUnis3
    :: S.Set (T.Text, T.Text)
    -> [P.Nss3Line]
    -> [P.Nss3Line]
removeTinyUnis3 bigUnis =
    filter (flip S.member bigUnis . nss3ToTup)


nss3ToTup :: P.Nss3Line -> (T.Text, T.Text)
nss3ToTup line =
    (P.n3UniName line, P.n3Subject line)


{-| It takes in the whole of the NSS3 parsed data, and outputs the
university and subject for each subject where the sample size is
greater than 40 for survey question 1.
-}
findBigNss3Unis :: [P.Nss3Line] -> S.Set (T.Text, T.Text)
findBigNss3Unis =
    S.fromList .
    nub .
    map nss3ToTup .
    filter ((> 40) .  P.n3SampleSize) .
    filter ((== 1) . P.n3Question)


{-| Given the names of the bigger universities in the NSS3
worksheet, it finds the universities in the NSS worksheet that are
included in it and have a sample size greater than 200.
-}
findBigNssUnis :: S.Set T.Text -> [P.NssLine] -> S.Set T.Text
findBigNssUnis bigNss3Unis =
    S.fromList .
    filter (`S.member` bigNss3Unis) .
    map P.nUniName .
    filter ((> 200) . P.nSampleSize)
