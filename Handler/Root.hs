{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root where

import Foundation
import qualified Data.Text as T
import Data.Text.Internal
import Random

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = getRootNR 0

getRootNR :: Integer -> Handler RepHtml
getRootNR n = do
    imagesWithIds <- runDB $ selectList ([]::[Filter Image]) [LimitTo 20, OffsetBy (fromEnum $ n*20)]
    let images = map snd imagesWithIds
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "Imgur directory listing"
        $(widgetFile "homepage")

getImageR :: ImageId -> Handler RepHtml
getImageR n = do
    maybeImage <- runDB $ get n
    case maybeImage of
        Just image -> redirectText RedirectPermanent (imageUrl image)
        Nothing -> notFound

getPartialR :: Text -> Handler RepHtml
getPartialR s = do
    imagesWithIds <- runDB $ selectList ([] :: [Filter Image]) []
    let images = filter (\image -> (s `T.isPrefixOf` imageUrl image)) (map snd imagesWithIds)
    if length images == 0
        then notFound
        else do
            ridx <- liftIO $ randomRIO (0, (length images) - 1)
            let image = images !! ridx
            redirectText RedirectPermanent (imageUrl image)
