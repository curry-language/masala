------------------------------------------------------------------------------
-- Global configurations for the Masala system
------------------------------------------------------------------------------

module Config.Masala
 where

import System.Environment ( getEnv )

import System.FilePath    ( (</>) )
import Data.Time

------------------------------------------------------------------------------
--- Is the current installation a test system?
--- As a default, it is a test system unless the environment variable
--- `MASALATEST` has value `no`.
--- In a test system, mails are not really sent but its contents is
--- just shown in the web page which sent it (see `Controller.Mail`).
--- Moreover, the source files of packages are not downloaded and
--- nothing is sent to CPM when a package is published.
isTestSystem :: IO Bool
isTestSystem = do
  masalatest <- getEnv "MASALATEST"
  return $ masalatest /= "no"

--- Email address of administrator:
adminEmail :: String
adminEmail = "masala@curry-lang.org"

--- The name of the main script of the Masala system.
baseCGI :: String
baseCGI = "run.cgi"

--- The URL of the main script of the module system
--- (used to generate external URLs for modules and master programs):
getBaseURL :: IO String
getBaseURL = do
  testsys <- isTestSystem
  return $ if testsys then "http://localhost/~mh/masala"
                      else "https://cpm.curry-lang.org/masala"

--- The URL of the main script of the module system
--- (used to generate external URLs for modules and master programs):
getMainScriptURL :: IO String
getMainScriptURL = fmap (++ ("/" ++ baseCGI)) getBaseURL

--- The system hash key used to encode passwords
--- (compare `System.Authentication`).
systemHashKey :: String
systemHashKey = "3masala25" -- change this key for every spicey instance

--- The data directory where the private data of Masala is stored.
masalaDataDir :: String
masalaDataDir = "data"

--- The SQLite3 database file of Masala.
masalaDBFile :: String
masalaDBFile = masalaDataDir </> "Masala2.db"

--- Log file for emails.
emailLogFile :: String
emailLogFile = masalaDataDir </> "EMAILLOG.txt"

--- The directory where the package specifications (`package.json`) are stored.
packageSpecDir :: String
packageSpecDir = masalaDataDir </> "packages"

--- The directory where the sources of uploaded packages are stored.
--- If it is empty, the sources are not downloaded.
getDownloadSourceDir :: IO String
getDownloadSourceDir = do
  testsys <- isTestSystem
  return $ if testsys then "" -- do not download in test system
                      else masalaDataDir </> "downloads"

--- The directory where the tar files of the sources of uploaded packages
--- are stored.
downloadTarDir :: String
downloadTarDir = "TARFILES"

--- This function returns the time, to which every validation token older than
--- that is invalid and supposed to be deleted.
invalidTime :: IO ClockTime
invalidTime = do
  ctime <- getClockTime
  return $ addMinutes (-10) ctime

------------------------------------------------------------------------------
-- Access to the CPM repository manager:

--- The URL to upload a package specification to CPM so that other
--- programmers can use them. The package specification must be provided
--- on stdin and the package's tar file must exist in the `downloadTarDir`.
cpmUploadURL :: String
cpmUploadURL = "https://cpm.curry-lang.org/cpm-upload-masala.cgi"

--- The base directory where the data of CPM is stored.
getCPMBaseDir :: IO String
getCPMBaseDir = do
  testsys <- isTestSystem
  return $ if testsys then "/home/mh/public_html/curry/cpm"
                      else "/net/medoc/home/cpm/public_html"

--- Documentation URL of a package version in the CPM repository.
packageURLinCPM :: String -> String -> String
packageURLinCPM pkgname version =
  let pkgid = pkgname ++ if null version then "" else '-' : version
  in "https://cpm.curry-lang.org/pkgs/" ++ pkgid ++ ".html"

------------------------------------------------------------------------------
