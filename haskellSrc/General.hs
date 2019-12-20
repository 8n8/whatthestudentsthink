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

{-# LANGUAGE DeriveGeneric #-}

module General
  ( Answer(..)
  , Nss2Input(..)
  , NssInput(..)
  , NssCodes(..)
  , Nss2Codes(..)
  ) where

{-| This module provides some datatypes required by several other modules.
-}

import GHC.Generics (Generic)
import qualified Data.IntSet as Si
import qualified Data.Text as T
import Data.Aeson (FromJSON)

{-| It contains a request for data from the internet. -}
data Nss2Input = Nss2Input
  { n2Subject :: Int
  , n2Unis :: Si.IntSet
  , n2Questions :: Si.IntSet
  } deriving (Generic, Show)

data NssInput = NssInput
  { nUnis :: Si.IntSet
  , nQuestions :: Si.IntSet
  } deriving (Generic, Show)

data NssCodes = NssCodes
  { cUnis :: [(T.Text, Int)]
  }

{-| It contains the lookup codes for university codes and names, and
subject area codes and names.
-}
data Nss2Codes = Nss2Codes
  { c2Unis :: [(T.Text, Int)]
  , c2Subjects :: [(T.Text, Int)]
  , c2SubjectsOffered :: [(Int, [Int])]
  , c2UnisOffering :: [(Int, [Int])]
  }

{-| These are for decoding and encoding NssInput and Nss2Input to and
from JSON.
-}
instance FromJSON NssInput

instance FromJSON Nss2Input

{-| The possible answers to the survey questions. -}
data Answer
  = DefinitelyAgree
  | MostlyAgree
  | NeitherAgreeNorDisagree
  | MostlyDisagree
  | DefinitelyDisagree
  | NoAnswer
  deriving Show

