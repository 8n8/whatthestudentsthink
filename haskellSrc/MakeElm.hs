module MakeElm (makeElm) where

makeElm :: B.ByteString -> B.ByteString -> Either T.Text T.Text
makeElm nssRaw nss3Raw = do
    nss <- parseNss nssRaw
    nss3 <- parseNss3 nss3Raw
    let
        bigNss3 = findBigNss3 nss3
        bigNss = findBigNss 

    
