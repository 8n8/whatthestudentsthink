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
{-# LANGUAGE DeriveGeneric #-}

{-| This is the main server.  First it reads and parses the data files 
containing the survey data.  It assigns integer codes for the universities
and subject areas and generates a file of Elm code so that the frontend
is aware of the codes.  It sets up routes for serving up the frontend
code, and for responding to json data requests.
-}

module Server where

import qualified General as G
import Data.ByteString.Lazy.Internal (ByteString)
import GHC.Generics (Generic)
import qualified Data.IntSet as Si
import qualified Data.Aeson as Json
import qualified Data.Text as T
import qualified SelectDataForChart
import qualified Web.Scotty as Scot
import ReadAndBasicProcess (readAndBasicProcess)
import qualified Parser as P
import qualified DevOrProduction as Dp

main :: IO ()
main = do
  parsed <- readAndBasicProcess
  case parsed of
    Left errMsg -> print errMsg
    Right (nss, nss2, _, _) ->
      respond2requests nss2 nss

{-| It sets up the routes for handling the requests from the
frontend.
-}
respond2requests :: [P.IntNss2Line] -> [P.IntNssLine] -> IO ()
respond2requests nss2 nss = do
  binPath <- Dp.binPath
  let indexhtml = binPath ++ "/index.html"
  let mainjs = binPath ++ "/main.js"
  let faviconico = binPath ++ "/favicon.ico"
  let elmjs = binPath ++ "/elm.js"
  let stylescss = binPath ++ "/styles.css"
  let abouthtml = binPath ++ "/about.html"
  let googlecheck = binPath ++ "/googleee2d16a05a2bb8f2.html"
  let aboutstylescss = binPath ++ "/aboutStyles.css"
  Dp.scotty $ do
    Scot.get "/" $ do
      topHeaders
      Scot.setHeader "Content-Type" "text/html; charset=utf-8"
      Scot.file indexhtml
    Scot.get "/favicon.ico" $ do
      subHeaders
      Scot.setHeader "Content-Type" "image/x-icon"
      Scot.file faviconico
    Scot.get "/main.js" $ do
      subHeaders
      Scot.setHeader "Content-Type" "application/javascript; charset=utf-8"
      Scot.file mainjs
    Scot.get "/elm.js" $ do
      subHeaders
      Scot.setHeader "Content-Type" "application/javascript; charset=utf-8"
      Scot.file elmjs
    Scot.get "/styles.css" $ do
      Scot.setHeader "Content-Type" "text/css; charset=utf-8"
      subHeaders
      Scot.file stylescss
    Scot.get "/about" $ do
      Scot.setHeader "Content-Type" "text/html; charset=utf-8"
      topHeaders
      Scot.file abouthtml
    Scot.get "/googleee2d16a05a2bb8f2.html" $ do
      Scot.setHeader "Content-Type" "text/html; charset=utf-8"
      topHeaders
      Scot.file googlecheck
    Scot.get "/aboutStyles.css" $ do
      Scot.setHeader "Content-Type" "text/css; charset=utf-8"
      subHeaders
      Scot.file aboutstylescss
    Scot.get "/nss/:queryJson" $ do
      jsonHeaders
      rawRequest <- Scot.param "queryJson"
      respondToNssDataRequest (decodeNss rawRequest) nss
    Scot.get "/nss2/:queryJson" $ do
      jsonHeaders
      rawRequest <- Scot.param "queryJson"
      respondToNss2DataRequest (decodeNss2 rawRequest) nss2

{-| Given a request for data from the NSS data set and the data,
it selects the data for the response and encodes it to JSON.
-}
respondToNssDataRequest :: Maybe G.NssInput -> [P.IntNssLine] -> Scot.ActionM ()
respondToNssDataRequest maybeRequest nss =
  case maybeRequest of
    Nothing ->
      Scot.json Output
        { err = "Could not parse input."
        , result = []
        }
    Just input ->
      Scot.json Output
        { err = ""
        , result = SelectDataForChart.nss input nss
        }

{-| It decodes the JSON in a request for data from the NSS2 data
set.
-}
decodeNss2 :: ByteString -> Maybe G.Nss2Input
decodeNss2 rawRequest =
  case Json.decode rawRequest of
    Nothing -> Nothing
    Just content ->
      if nss2InputOk content then
        Just content
      else Nothing

{-| It decodes a JSON request for data from the NSS data set. -}
decodeNss :: ByteString -> Maybe G.NssInput
decodeNss rawRequest =
  case Json.decode rawRequest of
    Nothing -> Nothing
    Just content ->
      if nssInputOk content then
        Just content
      else Nothing

{-| It does a quick reasonableness check on a data request for the
NSS data set.
-}
nssInputOk :: G.NssInput -> Bool
nssInputOk (G.NssInput us qs)
  | not $ inputSetOk 300 us = False
  | not $ inputSetOk 27 qs = False
  | otherwise = True

{-| It does a quick reasonableness check on a data request for the
NSS2 data set.
-}
nss2InputOk :: G.Nss2Input -> Bool
nss2InputOk (G.Nss2Input s us qs)
  | s < 0 && s > 50 = False
  | not $ inputSetOk 300 us = False
  | not $ inputSetOk 27 qs = False
  | otherwise = True

{-| It checks that the numbers in a set of Ints are all less than
a given limit.
-}
inputSetOk :: Int -> Si.IntSet -> Bool
inputSetOk maxVal s
  | Si.null s = True
  | Si.findMin s < 1 = False
  | Si.findMax s > maxVal = False
  | Si.size s > maxVal = False
  | otherwise = True

{-| It checks it was possible to decode the JSON request string.  If it
was then it calculates the data to send back and sends it.  If not
then it sends an error message.
-}
respondToNss2DataRequest
  :: Maybe G.Nss2Input -> [P.IntNss2Line] -> Scot.ActionM ()
respondToNss2DataRequest maybeRequest nss2Dat =
  case maybeRequest of
    Nothing ->
      Scot.json Output
        { err = "Could not parse input."
        , result = []
        }
    Just input ->
      Scot.json Output
        { err = ""
        , result = SelectDataForChart.nss2 input nss2Dat
        }

{-| Various headers intended to increase security somewhat.
-}
topHeaders :: Scot.ActionM ()
topHeaders = do
  Scot.setHeader "Content-Security-Policy" "script-src 'self' 'sha256-LY7oE7OZlrCiTGQMMFwKlDID0F8sf8t6FcUIyF06bjo='; default-src 'none'; connect-src 'self'; font-src https://fonts.googleapis.com https://fonts.gstatic.com data: ; img-src 'self'; object-src 'none'; style-src 'self' https://fonts.googleapis.com"
  Scot.setHeader "X-Frame-Options" "SAMEORIGIN"
  Scot.setHeader "X-XSS-Protection" "1; mode=block"
  Scot.setHeader "X-Content-Type-Options" "nosniff"
  Scot.setHeader "Referrer-Policy" "no-referrer-when-downgrade"
  Scot.setHeader "Server" ""
  Scot.setHeader "Strict-Transport-Security" "max-age=63072000"

subHeaders :: Scot.ActionM ()
subHeaders =
  Scot.setHeader "Server" ""

jsonHeaders :: Scot.ActionM ()
jsonHeaders = do
    Scot.setHeader "Content-Type" "application/json; charset=utf-8"
    subHeaders
    apiCSP

apiCSP :: Scot.ActionM ()
apiCSP =
  Scot.setHeader "Content-SecurityPolicy" "default-src 'none'; frame-ancestors 'none'"

{-| It contains the response to a data request. -}
data Output = Output
  { err :: T.Text
  , result :: [[Int]]
  } deriving (Generic, Show)

instance Json.ToJSON Output where
  toEncoding = Json.genericToEncoding Json.defaultOptions
