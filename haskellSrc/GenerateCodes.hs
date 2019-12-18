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

module GenerateCodes where

{-| This module analyses the data files and creates the Elm code
containing the various lookup dictionaries for the front end.
-}

import qualified Data.Text.IO as Tio
import MakeElmCode (elmify)
import ReadAndBasicProcess (readAndBasicProcess)

main :: IO ()
main = do
  eitherResult <- readAndBasicProcess
  case eitherResult of
    Left err -> print err
    Right result ->
      Tio.writeFile "elmSrc/Data.elm" $ elmify result
