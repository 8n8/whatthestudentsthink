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

module ReadAndBasicProcess (readAndBasicProcess) where

{-| It reads in the data files, parses them, and does basic preprocessing,
such as removing small institutions.
-}

import qualified Data.ByteString
import Data.Text.Encoding (decodeUtf8)
import qualified Parser as P
import qualified Data.Set as S
import qualified Data.Text as T
import qualified MakeCodes
import Data.List (nub)
import qualified Data.Map as Map
import qualified General as G
import qualified DevOrProduction as Dp

{-| It reads a file to Text, using UTF8 encoding. -}
readUtf8 :: String -> IO T.Text
readUtf8 fileName = decodeUtf8 <$> Data.ByteString.readFile fileName

{-| It reads in the two CSV data files, parses them, removes the small
universities and creates the integer codes.
-}
readAndBasicProcess
  :: IO ( Either String
         ( [P.IntNssLine]
         , [P.IntNss2Line]
         , G.NssCodes
         , G.Nss2Codes
         ))
readAndBasicProcess = do
  binPath <- Dp.binPath
  nssOverallContents <- readUtf8 $ binPath ++ "/nss.csv"
  nss2Contents <- readUtf8 $ binPath ++ "/nss2.csv"
  case P.nss2 nss2Contents of
    Left parseErr -> return $ Left $ show parseErr
    Right nss2 ->
      case P.nss nssOverallContents of
        Left parseErr -> return $ Left $ show parseErr
        Right nss -> do
          let bigNss2Unis = findBigNss2Unis nss2
          let bigNssUnis = findBigNssUnis (S.map fst bigNss2Unis) nss
          let wordyNss = removeTinyUnis bigNssUnis nss
          let wordyNss2 = removeTinyUnis2 bigNss2Unis nss2
          let nss2Codes = MakeCodes.nss2 wordyNss2
          let nssCodes = MakeCodes.nss wordyNss
          case (nss2ToInt nss2Codes wordyNss2, nssToInt nssCodes wordyNss) of
            (Just nss2Int, Just nssInt) -> return $
                Right (nssInt, nss2Int, nssCodes, nss2Codes)
            _ -> return $
                Left "Could not convert universities and subjects to ints."

{-| Given a list of Maybes, it returns Nothing if any of them is Nothing,
and the values if they are all Just.
-}
fromMaybes :: [Maybe a] -> Maybe [a]
fromMaybes [] = Just []
fromMaybes (Nothing : _) = Nothing
fromMaybes (Just a : rest) = fmap (a :) (fromMaybes rest)

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
    Just uni -> Just
      P.IntNssLine
        { P.iUni = uni
        , P.iqNum = P.nQNum line
        , P.iMinConf = P.nMinConf line
        , P.iValue = P.nValue line
        , P.iMaxConf = P.nMaxConf line
        , P.iSampleSize = P.nSampleSize line
        }
    Nothing -> Nothing

{-| It takes in the integer codes for universities and subjects for the
'NSS2' worksheet, and the parsed data.  Its output is the same data but with
the subject and university names replaced with their integer codes.
-}
nss2ToInt :: G.Nss2Codes -> [P.Nss2Line] -> Maybe [P.IntNss2Line]
nss2ToInt codes dat =
  let
    uniMap = Map.fromList $ G.c2Unis codes
    subjMap = Map.fromList $ G.c2Subjects codes
  in
    fromMaybes . map (oneNss2ToInt uniMap subjMap) $ dat

{-| It converts the university and subject names in one line from the 'NSS2'
worksheet into their integer codes.
-}
oneNss2ToInt
  :: Map.Map T.Text Int
  -> Map.Map T.Text Int
  -> P.Nss2Line
  -> Maybe P.IntNss2Line
oneNss2ToInt uniMap subjMap line =
  case ( Map.lookup (P.n2UniName line) uniMap
       , Map.lookup (P.n2Subject line) subjMap) of
    ( Just uni, Just subj ) -> Just
        P.IntNss2Line
          { P.i2Uni = uni
          , P.i2Subject = subj
          , P.i2Question = P.n2Question line
          , P.i2MinConf = P.n2MinConf line
          , P.i2Value = P.n2Value line
          , P.i2MaxConf = P.n2MaxConf line
          , P.i2SampleSize = P.n2SampleSize line
          }
    _ -> Nothing


{-| It takes in a list of all the larger university names and the whole
of the parsed data from the 'NSS' worksheet, and removes all the small
universities from it.  This was done because it was found that the charts
were cluttered up with lots of very small universities.
-}
removeTinyUnis :: S.Set T.Text -> [P.NssLine] -> [P.NssLine]
removeTinyUnis bigUnis =
  filter (flip S.member bigUnis . P.nUniName)

{-| Removes small universities from the 'NSS2' data. -}
removeTinyUnis2 :: S.Set (T.Text, T.Text) -> [P.Nss2Line] -> [P.Nss2Line]
removeTinyUnis2 bigUnis =
  filter (flip S.member bigUnis . nss2ToTup)

nss2ToTup :: P.Nss2Line -> (T.Text, T.Text)
nss2ToTup line = (P.n2UniName line, P.n2Subject line)

{-| It takes in the whole of the NSS2 parsed data, and outputs the university
and subject for each subject where the sample size is greater than 40 for
survey question 1.
-}
findBigNss2Unis :: [P.Nss2Line] -> S.Set (T.Text, T.Text)
findBigNss2Unis =
    S.fromList . nub . map nss2ToTup . filter ((> 40) . P.n2SampleSize) .
      filter ((== 1) . P.n2Question)

{-| Given the names of the bigger universities in the NSS2 worksheet, it
finds the universities in the NSS worksheet that are included in it and
have a sample size greater than 200.
-}
findBigNssUnis :: S.Set T.Text -> [P.NssLine] -> S.Set T.Text
findBigNssUnis bigNss2Unis =
  S.fromList . filter (`S.member` bigNss2Unis) .
    map P.nUniName . filter ((> 200) . P.nSampleSize)
