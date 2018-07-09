{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

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
import Handlers

main :: IO()
main = do 
 run 8000 appl

type MangaAPI ="all" :> Get '[HTML] [MangaInfo] :<|>
                 Capture "manga" String :> Get '[HTML] [MangaChapter] :<|>
                 Capture "manga" String :> Capture "ch" Int
                    :> Capture "pg" Int :> Get '[HTML] PageUrl

server :: Server MangaAPI
server = titles :<|> chapters :<|> pages

mAPI :: Proxy MangaAPI
mAPI = Proxy

appl :: Application
appl = serve mAPI server




