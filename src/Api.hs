{-# LANGUAGE OverloadedStrings #-}

module Api
( getMangaTitles
, getMangaChapters
, getMangaImg
, getChapterLen
) where

import Data.List
import Text.HTML.Scalpel

mangaTitlesURL = "http://www.mangareader.net/alphabetical"
mangaBaseURL   = "http://www.mangareader.net/"

getMangaTitles :: IO (Maybe [(String, String)])
getMangaTitles = scrapeURL mangaTitlesURL titles
 where
  titles :: Scraper String [(String, String)]
  titles = chroots (("ul" @: ["class" @= "series_alpha"]) // "li") title

  title :: Scraper String (String, String)
  title = do
   titleText <- text "a"
   mangaURL  <- attr "href" "a"
   return (titleText, mangaURL)

getMangaChapters :: String -> IO (Maybe [(String, String)])
getMangaChapters title = scrapeURL specificURL chapters
 where
  chapters :: Scraper String [(String, String)]
  chapters = chroots (("table" @: ["id" @= "listing"]) // "tr") chapter

  chapter :: Scraper String (String, String)
  chapter = do
   chapterName <- text "td"
   chapterURL  <- attr "href" "a"
   return (dropWhile (== '\n') chapterName, chapterURL)

  specificURL :: String
  specificURL = mangaBaseURL ++ title

getMangaImg :: String -> Int -> Int -> IO (Maybe String)
getMangaImg title ch pg = scrapeURL specificURL img
 where
  img :: Scraper String String
  img = chroot (("div" @: ["id" @= "imgholder"]) // "img") i

  i :: Scraper String String
  i = do
   imgSrc <- attr "src" anySelector
   return imgSrc
  
  specificURL :: String
  specificURL = mangaBaseURL ++ title ++ "/" ++ show ch ++ "/" ++ show pg

getChapterLen :: String -> Int -> IO (Maybe String)
getChapterLen title ch = scrapeURL specificURL len
 where
  len :: Scraper String String
  len = chroot ("div" @: ["id" @= "selectpage"]) l

  specificURL :: String
  specificURL = concat [mangaBaseURL, title, "/", show ch]

  l :: Scraper String String
  l = do
   unprocessed <- text "div"
   return $ last $ split " " unprocessed
   where
    split :: [Char] -> [Char] -> [[Char]]
    split d = foldr f [[]]
     where f c (xs:xss) = maybe ((c:xs):xss) (\ys -> []:ys:xss) $
            stripPrefix d (c:xs) 









