{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O2 #-}

module Main where

import TwilioParser

import Text.HandsomeSoup
import Text.XML.HXT.Core
import Control.Lens
import Happstack.Lite
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.Trans
import Text.StringLike
import Data.String
import Text.HTML.TagSoup
import qualified Data.Text.Lazy as T
import Network.HTTP hiding (Response)

baseUrl :: String
baseUrl = "http://www.goodreads.com"

searchUrl :: String -> String
searchUrl x = concat [baseUrl, "/search?utf8=%E2%9C%93&search_type=books&query=", x]

grabFirstSearchUrlForTitle :: String -> IO (Maybe String)
grabFirstSearchUrlForTitle title =  
  do result <- runX $ (fromUrl $ searchUrl title) >>> css "a.bookTitle" ! "href"
     return $ (result ^? ix 0)

extractText :: (Monad m, IsString a, Text.StringLike.StringLike a) => [Char] -> [Tag a] -> m a
extractText i tags = do let f = (sections (~== (i :: String))) tags
                        case f of
                          [] -> return "Could not find item"
                          (l:_) -> do case [r | TagText r <- l] of
                                        [] -> return "Could not extract data from Goodreads"
                                        (x:_)  -> return x 

grabRatingsForSearchPath :: Maybe String -> IO String
grabRatingsForSearchPath Nothing = return "Could not find book :("
grabRatingsForSearchPath (Just path) =
  do let html = simpleHTTP (getRequest (baseUrl ++ path)) >>= getResponseBody
     tags <- fmap parseTags html
     rating <- extractText "<span itemprop=ratingValue>" tags
     name   <- extractText "<h1 id=bookTitle>" tags
     return $ concat ["Name: ", name, " | Rating: ", rating]

grabRatings :: String -> IO String
grabRatings x =
  do result <- grabFirstSearchUrlForTitle (urlEncode x)
     rating <- grabRatingsForSearchPath result
     return rating

twilioSMSResponse :: String -> String
twilioSMSResponse x = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" ++
                      "<Response>" ++
                        "<Message>" ++ x ++ "</Message>" ++
                      "</Response>"

findReview :: ServerPart Response
findReview = do setHeaderM "Content-Type" "text/xml"
                body <- lookText "Body"
                from <- lookText "From"
                let res = InboundSMS (SMSBody [T.unpack from] [T.unpack body])
                liftIO $ putStrLn $ show res
                reviewScore <- liftIO $ grabRatings $ concat $ (text.smsBody) res
                (textResponse.twilioSMSResponse) reviewScore
                  where textResponse = ok.toResponse.L.pack

webapp :: ServerPart Response
webapp = msum [
    dir "reviews" $ findReview
  ]

main :: IO ()
main =
  do serve Nothing webapp 

