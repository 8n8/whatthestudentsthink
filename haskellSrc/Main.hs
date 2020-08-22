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

module Main (main) where

{-| This module processes the raw data files and hard-codes it into
the Elm code in the 'Data' module.
-}

import qualified Data.Text.IO as Tio
import qualified Data.ByteString as B
import MakeElm (makeElm)


main :: IO ()
main = do
    nssRaw <- B.readFile "dataFiles/nss.csv"
    nss3Raw <- B.readFile "dataFiles/nss3.csv"
    case makeElm nssRaw nss3Raw of
        Left err ->
            print err

        Right result ->
            Tio.writeFile "elmSrc/Data.elm" result
