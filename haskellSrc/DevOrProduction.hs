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
module DevOrProduction where

{-| It makes it easy to switch between development and production
builds.  In development it is convenient to run the server on
localhost, with http.  In production it runs on port 443 with https.

To switch from one mode to the other, just change the 'status'
variable.
-}
import           System.Environment (getExecutablePath)
import qualified Web.Scotty         as Scot
import qualified Web.Scotty.TLS     as Tls

data DevOrProduction
  = Dev
  | Production

status :: DevOrProduction
status = Production

{-| Given a file path like "/a/b/c/d.txt", it removes the file
name, giving "/a/b/c".
-}
stripFileName :: String -> String
stripFileName = reverse . stripReverseFileName . reverse

{-| Given a file path in reverse, like "txt.d/c/b/a/", it strips
off the file name, giving "c/b/a/".
-}
stripReverseFileName :: String -> String
stripReverseFileName ""            = ""
stripReverseFileName ('/':remains) = remains
stripReverseFileName (_:remains)   = stripReverseFileName remains

{-| The path of the https keys.  In this case, LetsEncrypt is used. -}
certPath :: String
certPath = "/etc/letsencrypt/live/whatthestudentsthink.uk/"

privkey :: String
privkey = certPath ++ "privkey.pem"

fullchain :: String
fullchain = certPath ++ "fullchain.pem"

{-| The function for making the http routes.  It is different depending
on whether build is for development or production.
-}
scotty :: Scot.ScottyM () -> IO ()
scotty =
  case status of
    Dev        -> Scot.scotty 3000
    Production -> Tls.scottyTLS 443 privkey fullchain

{-| The data files, like the html files, are stored in a directory called
'dataFiles'.  In production, this is kept with the binaries.  In development,
it is kept in the root directory of the source code.
-}
binPath :: IO String
binPath =
  case status of
    Dev        -> return "dataFiles"
    Production -> fmap ((++ "/dataFiles") . stripFileName) getExecutablePath
