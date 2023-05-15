--------------------------------------------------------------------------
--- This main module of a Spicey application.
--------------------------------------------------------------------------

module Main where

import HTML.Base
import HTML.WUI

import Config.ControllerMapping
import Config.RoutesData
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

--- Main function: call the dispatcher
main :: IO HtmlPage
main = dispatcher
