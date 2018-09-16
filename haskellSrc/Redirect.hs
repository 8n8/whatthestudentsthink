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

{-| This is a tiny server that listens on port 80 and redirects everything
to port 443.
-}
module Redirect where

import           Network.HTTP.Types.Status (status301)
import qualified Web.Scotty                as Scot

main :: IO ()
main =
  Scot.scotty 80 $
  Scot.get "/" $ do
    Scot.setHeader "Server" ""
    Scot.status status301
    Scot.redirect "https://whatthestudentsthink.uk"
