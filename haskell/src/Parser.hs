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

module Parser
  ( nss3
  , nss
  , NssLine(..)
  , Nss3Line(..)
  , IntNssLine(..)
  , IntNss3Line(..)
  , ParseError
  ) where


{-| It provides two parsers, one for the worksheet "NSS" in the data
spreadsheet and one for the worksheet "NSS3".

The reason for building a custom parser and not just using a standard
CSV parser is that I would have had to have made custom parsers for
a lot of the cells anyway, as they are not standard.  For example, the
question number column does not just have a number, it has things like
"Q3" or "NHS1".
-}


import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as P
import Data.Void ( Void )


{-| A parsed line in the 'NSS' worksheet. -}
data NssLine
    = NssLine
        { nUniName :: !T.Text
        , nQNum :: !Int
        , nMinConf :: !Int
        , nValue :: !Int
        , nMaxConf :: !Int
        , nSampleSize :: !Int
        }


{-| A parsed line in the NSS worksheet, but with the strings converted to their
integer codes.
-}
data IntNssLine
    = IntNssLine
        { iUni :: !Int
        , iqNum :: !Int
        , iMinConf :: !Int
        , iValue :: !Int
        , iMaxConf :: !Int
        , iSampleSize :: !Int
        }


type Parser
    = M.Parsec Void T.Text


type ParseError
    = M.ParseErrorBundle T.Text Void


{-| It contains a single data point from the 'NSS3' worksheet, after parsing. -}
data Nss3Line
    = Nss3Line
        { n3UniName :: !T.Text
        , n3Subject :: !T.Text
        , n3Question :: !Int
        , n3MinConf :: !Int
        , n3Value :: !Int
        , n3MaxConf :: !Int
        , n3SampleSize :: !Int
        } deriving Show


{-| A parsed line in the NSS3 worksheet, but with the strings converted to their
integer codes.
-}
data IntNss3Line
    = IntNss3Line
        { i3Uni :: !Int
        , i3Subject :: !Int
        , i3Question :: !Int
        , i3MinConf :: !Int
        , i3Value :: !Int
        , i3MaxConf :: !Int
        , i3SampleSize :: !Int
        }


{-| Parses one line in the 'NSS' worksheet. -}
parseNssLine :: Parser (Maybe NssLine)
parseNssLine = do
    uniName <- parseIdAndUni
    maybeQNum <- parseQuestionNum M.<|> parseNhs
    (minConf, value, maxConf) <- parseQColsAndMinValMax
    sampleSize <- M.try parseInt
    comma
    _ <- P.char 'Y' M.<|> return ' '
    voidEol M.<|> M.eof
    return $ case maybeQNum of
        Just qNum ->
            Just $
                NssLine uniName qNum minConf value maxConf sampleSize

        _ ->
            Nothing


{-| Parses the three numbers needed from each row: the ends of the 95%
confidence interval and the score.
-}
parseQColsAndMinValMax :: Parser (Int, Int, Int)
parseQColsAndMinValMax = do
    comma
    ignoreCells 6
    minConf <- parsePercent
    comma
    value <- parsePercent
    comma
    maxConf <- parsePercent
    comma
    ignoreCell
    return (minConf, value, maxConf)


{-| The input is the whole of the 'NSS' worksheet in CSV form.  The output
is an error or the parsed data.
-}
nss :: T.Text -> Either ParseError [NssLine]
nss =
    M.parse parseNss "nssFile"


{-| The input is the whole of the 'NSS3' worksheet in CSV form.  The output
is an error or the parsed data.
-}
nss3 :: T.Text -> Either ParseError [Nss3Line]
nss3 =
    M.parse parseNss3 "nss3File"


{-| The parser for the 'NSS' worksheet. -}
parseNss :: Parser [NssLine]
parseNss = do
    firstFourLines
    contentsMaybes <- M.some parseNssLine
    return $ catMaybes contentsMaybes


{-| The parser for the NSS3 worksheet. -}
parseNss3 :: Parser [Nss3Line]
parseNss3 = do
    firstFourLines
    contentsMaybes <- M.some parseNss3Line
    return $ catMaybes contentsMaybes


{-| The first four lines in each worksheet are column headers and titles and
things that are not needed.  This parser chews through them and ignores the
content.
-}
firstFourLines :: Parser ()
firstFourLines = do
    parseAnyLine
    parseAnyLine
    parseAnyLine
    parseAnyLine


{-| Parses a line and ignores its content. -}
parseAnyLine :: Parser ()
parseAnyLine = do
    _ <- M.some P.printChar
    _ <- P.eol
    return ()


{-| Parses a comma. -}
comma :: Parser ()
comma = do
    _ <- P.char ','
    return ()


{-| Parses a cell and ignores its content. -}
ignoreCell :: Parser ()
ignoreCell = do
    _ <- M.takeWhileP Nothing (/= ',')
    comma
    return ()


{-| Parses a subject without quote marks around it.  Some subjects areas
contain commas in the name, so are quoted to avoid confusion with the
commas that separate the cells.
-}
parsePlainSubject :: Parser T.Text
parsePlainSubject = do
    parseText


{-| Parses a subject enclosed in quotation marks. -}
parseQuotedSubject :: Parser T.Text
parseQuotedSubject = do
    _ <- P.char '\"'
    parseEndOfQuote


{-| Parses a university name enclosed in quotation marks. -}
parseQuotedUni :: Parser T.Text
parseQuotedUni = do
    _ <- P.char '\"'
    parseEndOfQuote


{-| Parses some quoted text, starting after the opening quote. -}
parseEndOfQuote :: Parser T.Text
parseEndOfQuote = do
    txt <- M.takeWhileP Nothing (/= '\"')
    _ <- P.char '\"'
    return txt


{-| Parses the name of a university.  It should work whether it is
quoted or not.
-}
parseUniName :: Parser T.Text
parseUniName =
    M.try parseQuotedUni M.<|> parseText


{-| It parses n cells, ignoring their content. -}
ignoreCells :: Int -> Parser ()
ignoreCells 0 = return ()
ignoreCells n = do
    ignoreCell
    ignoreCells (n-1)


{-| It parses the university ID number and ignores it, and then
parses the university name that is in the next column.
-}
parseIdAndUni :: Parser T.Text
parseIdAndUni = do
    _ <- M.takeWhileP Nothing isInt
    comma
    uniName <- parseUniName
    comma
    return uniName


parseSubjectCode :: Parser ()
parseSubjectCode = do
    _ <- M.takeWhileP Nothing notComma
    comma


{-| It parses one line of data in the 'NSS3' worksheet. -}
parseNss3Line :: Parser (Maybe Nss3Line)
parseNss3Line = do
    uniName <- parseIdAndUni
    parseSubjectCode
    subject <- M.try parseQuotedSubject M.<|> parsePlainSubject
    comma
    level <- parseText
    comma
    maybeQNum <- parseQuestionNum M.<|> parseNhs
    (minConf, value, maxConf) <- parseQColsAndMinValMax
    sampleSize <- parseInt
    comma
    _ <- P.char 'Y' M.<|> return ' '
    voidEol M.<|> M.eof
    return $ case (level == "First degree", maybeQNum) of
        (True, Just qNum) ->
            Just $
                Nss3Line uniName subject qNum minConf value maxConf sampleSize

        _ ->
            Nothing


{-| It parses an end of line. -}
voidEol :: Parser ()
voidEol = do
    _ <- P.eol
    return ()


{-| It parses any string, only stopping when it reaches a comma. -}
parseText :: Parser T.Text
parseText =
    M.takeWhileP Nothing (/= ',')


{-| It parses an integer. -}
parseInt :: Parser Int
parseInt = do
    digits <- M.takeWhileP Nothing isInt
    return $ txt2int digits


notComma :: Char -> Bool
notComma ch =
    ch /= ','


{-| Checks that a character is a number. -}
isInt :: Char -> Bool
isInt ch =
    ch `elem` intStr


{-| The characters that are accepted as integers. -}
intStr :: String
intStr =
    "0123456789"


{-| Converts a string containing an integer into an Int.  This is
unsafe in that it will fail if the string contains any other characters,
but this is OK in this instance since it is only used where the parser
has already checked it is valid.
-}
txt2int :: T.Text -> Int
txt2int =
    read . T.unpack


{-| Some of the question numbers are specific to NHS courses.  These are not
needed in this case.  This parser parses them and ignores the content.
-}
parseNhs :: Parser (Maybe Int)
parseNhs = do
    _ <- P.string "NHS" M.<|> P.string "Scale"
    oneOrTwoDigits M.<|> scale
    return Nothing


scale :: Parser ()
scale = do
    _ <- P.string "NHS"
    return ()


oneOrTwoDigits :: Parser ()
oneOrTwoDigits =
    M.try $ do
        _ <- P.digitChar
        _ <- P.digitChar M.<|> return ' '
        return ()


{-| The question numbers are the letter 'Q' followed by an integer between
1 and 27.  This parser extracts the number.
-}
parseQuestionNum :: Parser (Maybe Int)
parseQuestionNum = do
    _ <- P.char 'Q'
    Just <$> parseInt


{-| This parses percentages with a '%' symbol after them, like '10%'. -}
parsePercent :: Parser Int
parsePercent = do
    num <- parseInt
    _ <- P.char '%'
    return num
