{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module CustomData
( Person (..)
, PageUrl (..)
, MangaInfo (..)
, MangaChapter (..)) where

--import Data.Maybe
--import Servant.API
--import Servant
--import Network.Wai.Handler.Warp
--import Api
--import Control.Monad.IO.Class
--import Servant.HTML.Lucid
import Data.Text.Conversions
import Lucid.Html5
import GHC.Generics
import Lucid.Base
import Data.Text.Internal
import Data.Monoid


data Person = Person
 { firstName :: String
 , lastName :: String
 } deriving Generic

data PageUrl = PageUrl
 { mangaName :: String
 , chapter :: Int
 , page :: Int
 , maxPage :: Int
 , source :: Data.Text.Internal.Text } deriving Generic

data MangaInfo = MangaInfo
 { name :: Data.Text.Internal.Text
 , urlLink :: Data.Text.Internal.Text} deriving Generic

data MangaChapter = MangaChapter
 { chName :: Data.Text.Internal.Text
 , chUrlLink :: String } deriving Generic

instance ToHtml MangaChapter where
 toHtml ch =
  div_ [class_ "chapter entry"] $ do
   a_ [href_ url] $ toHtml $ chName ch
   where
    url = toText $ chUrlLink ch ++ "/1"

 toHtmlRaw = toHtml

instance ToHtml [MangaChapter] where
 toHtml xs =
  (a_ [href_ $ toText allUri] $ toHtml goBack)
  <>
  foldMap toHtml xs
  where
   allUri :: String
   allUri = "/all"

   goBack :: String
   goBack = "<- GO BACK TO MANGA LIST"

 toHtmlRaw = toHtml

instance ToHtml MangaInfo where
 toHtml mi =
  div_ [class_ "manga entry"] $ do
    a_ [href_ $ urlLink mi] $ toHtml $ name mi

 toHtmlRaw = toHtml

instance ToHtml [MangaInfo] where
 toHtml xs =
  foldMap toHtml xs

 toHtmlRaw = toHtml

instance ToHtml PageUrl where
 toHtml p =
  (div_ [class_ "back link"] $ do
          a_ [href_ mangaUri] $ toHtml $ toText goBack)
  <>
  (div_ [class_ "container"] $
          img_ [src_ (source p)])
  <>
  (if page p > 1 && page p < maxPage p then prevHtml <> nextHtml else
     (if page p <= 1 then nextHtml else prevHtml))
  where
   mangaUri = toText $ "/" ++ (mangaName p)

   prevHtml = form_ [action_ $ toText prevUri] $ do
                  button_ [type_ "submit"] $ toHtml $ toText prevTxt

   nextHtml = form_ [action_ $ toText nextUri] $ do
                  button_ [type_ "submit"] $ toHtml $ toText nextTxt

   goBack :: String
   goBack = "<- GO BACK TO CHAPTERS LIST"

   prevTxt :: String
   prevTxt = "Prev"

   nextTxt :: String
   nextTxt = "Next"

   prevUri :: String
   prevUri =
     concat ["/", mangaName p, "/", show $ chapter p, "/",  show $ (page p) - 1]

   nextUri :: String
   nextUri =
     concat ["/", mangaName p, "/", show $ chapter p, "/", show $ (page p) + 1]

 toHtmlRaw = toHtml





 
