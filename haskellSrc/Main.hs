module Main (main) where

import qualified Data.Text.IO as Tio
import qualified Data.ByteString as B
import MakeElm (makeElm)

main :: IO ()
main = do
    nssRaw <- B.readFile "nss.csv"
    nss3Raw <- B.readFile "nss3.csv"
    case makeElm nssRaw nss3Raw of
        Left err ->
            Tio.putStrLn err

        Right elm ->
            Tio.writeFile "elm/src/Data.elm" elm
