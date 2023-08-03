--------------------------------------------------------------------------
--- This main module of a Spicey application.
--------------------------------------------------------------------------

module Main where

import Data.List

import Data.Time
import HTML.Base
import HTML.Parser ( readHtmlFile )
import HTML.WUI
import System.Directory
import System.FilePath ( (</>) )
import Text.CSV

import Config.ControllerMapping
import Config.RoutesData
import Controller.Upload ( uploadByName )
import Model.Masala2
import Model.Queries
import System.Routes
import System.Processes
import System.Spicey
import View.Version       ( leqPkgVersion )

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
    ["csv"]     -> showMasalaDBAsCSV
    ["savedb"]  -> saveMasalaDB
    ["about"]   -> readFile "about.html" >>= getPage . (:[]) . BaseText
    ["imprint"] -> readFile "imprint.html" >>= getPage . (:[]) . BaseText
    ["privacy"] -> readFile "privacy.html" >>= getPage . (:[]) . BaseText
    ["UploadBy",login,passwd,publish,force,package]
      -> do upmsg <- uploadByName (urlencoded2string login)
                                  (urlencoded2string passwd)
                                  (urlencoded2string package)
                                  (read publish)
                                  (read force)
            return $ answerEncText "utf-8" $
              either ("ERROR: " ++) id upmsg
    _ ->  dispatcher

-----------------------------------------------------------------------------
--- Saves the database as term files in the `data` area of Masala.
--- To invoke this operation from a shell, one can use the command
---
---     > curl -s "https://cpm.curry-lang.org/masala/run.cgi?savedb"
---
saveMasalaDB :: IO HtmlPage
saveMasalaDB = do
  lt <- getLocalTime
  let times = concatMap show2 [ctYear lt, ctMonth lt, ctDay lt] ++ "-" ++
              concatMap show2 [ctHour lt, ctMin lt, ctSec lt]
      savedir = "data" </> "SAVED" </> times
  createDirectoryIfMissing True savedir
  saveDBTo savedir
  return $ answerEncText "utf-8" $ "Database saved into '" ++ savedir ++ "'.\n"
 where
  show2 i = if i < 10 then '0' : show i else show i

--- Shows the database in CSV format as used when initializing Masala
--- (see module `Model.Initialization`).
--- For instance, one can write the current database into a CSV file by
---
---     > curl -s "https://cpm.curry-lang.org/masala/run.cgi?csv" > allpkgs.csv
---
showMasalaDBAsCSV :: IO HtmlPage
showMasalaDBAsCSV = do
  versions <- runQ queryAllVersions
  pkgs     <- runQ (mapM getVersioningPackage versions)
  csv <- mapM pkgVers2CSV (sortBy leqPkgVersion (zip pkgs versions))
  return $ answerEncText "utf-8" $ showCSV csv
 where
  pkgVers2CSV (pkg,vers) = do
    deppkgs  <- runJustT (getDependingVersionPackages vers)
    expmods  <- runJustT (getExportingVersionCurryModules vers)
    cats     <- runQ $ getPackageVersionCategories pkg vers
    return [ packageName pkg, versionVersion vers, versionDescription vers
           , show (Just (toUTCTime (versionUploadDate vers)))
           , show (sort (map packageName deppkgs))
           , show (sort (map curryModuleName expmods))
           , show (sort (map categoryName cats))]

-----------------------------------------------------------------------------
