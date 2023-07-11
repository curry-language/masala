--------------------------------------------------------------------------
--- This main module of a Spicey application.
--------------------------------------------------------------------------

module Main where

import Data.List

import HTML.Base
import HTML.Parser ( readHtmlFile )
import HTML.WUI

import Config.ControllerMapping
import Config.RoutesData
import Controller.Upload ( uploadByName )
import System.Routes
import System.Processes
import System.Spicey

dispatcher :: IO HtmlPage
dispatcher = do
  -- get url
  (url,ctrlparams) <- getControllerURL
  
  controller <- nextControllerRefInProcessOrForUrl url >>=
                maybe displayUrlError getController

  page <- getPage controller
  saveLastUrl (url ++ concatMap ("/"++) ctrlparams)
  return page

--- Main function: check specific URL parameters and call the dispatcher
main :: IO HtmlPage
main = do
  params <- fmap (splitOn "/") getUrlParameter
  case params of
    ["About"] -> readHtmlFile "about.html" >>= getPage
    ["UploadBy",login,passwd,package]
      -> do uptxt <- uploadByName (urlencoded2string login)
                                  (urlencoded2string passwd)
                                  (urlencoded2string package)
            return $ answerEncText "utf-8"uptxt
    _ ->  dispatcher

