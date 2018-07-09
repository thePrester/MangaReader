{-# LANGUAGE OverloadedStrings #-}

module Handlers
( titles
, chapters
, pages) where

import Data.Maybe
import Servant.API
import Servant
import Network.Wai.Handler.Warp
import Api
import Control.Monad.IO.Class
import Servant.HTML.Lucid
import Data.Text.Conversions
import Lucid.Html5
--import GHC.Generics
import Lucid.Base
--import Data.Text.Internal
--import Data.Monoid
import CustomData

{-
 Handler for '/all' URI path
-}
titles :: Handler [MangaInfo]        
titles = do
 mangaTitles <- liftIO getMangaTitles
 return $ map (\(f,s) -> MangaInfo (toText f) (toText s)) $ fromJust mangaTitles 

{-
 Handler for '/(manga)' URI path
-}
chapters :: String -> Handler [MangaChapter]
chapters manga = do
 mangaChapters <- liftIO $ getMangaChapters manga
 return $ map (\(f,s) -> MangaChapter (toText f) s) $ fromJust mangaChapters

{-
 Handler for '/(manga)/(chapter)/(page)' URI path
-}
pages :: String -> Int -> Int -> Handler PageUrl
pages manga ch pg = do
 pageURL <- liftIO $ getMangaImg manga ch pg
 maxPg <- liftIO $ getChapterLen manga ch
 return $ PageUrl manga ch pg (read $ fromJust maxPg) $ toText $ fromJust pageURL




