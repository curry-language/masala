------------------------------------------------------------------------------
-- Global configurations for the Masala system
------------------------------------------------------------------------------

module Config.Masala
 where

import System.FilePath ( (</>) )

------------------------------------------------------------------------------
--- Is the current installation a test system?
--- In a test systems, mails are not really sent but its contents is
--- just shown in the web page which sent it (see `Controller.Mail`).
--- Moreover, the source files of packages are not downloaded and
--- nothing is sent to CPM when a package is published.
testSystem :: Bool
testSystem = True

--- Email address of administrator:
adminEmail :: String
adminEmail = "masala@curry-lang.org"

--- The name of the main script of the Masala system.
baseCGI :: String
baseCGI = "run.cgi"

--- The URL of the main script of the module system
--- (used to generate external URLs for modules and master programs):
baseURL :: String
baseURL | testSystem = "http://localhost/~mh/masala2"
        | otherwise  = "https://cpm.curry-lang.org/masala"

--- The URL of the main script of the module system
--- (used to generate external URLs for modules and master programs):
mainScriptURL :: String
mainScriptURL = baseURL ++ "/" ++ baseCGI

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
downloadSourceDir :: String
downloadSourceDir | testSystem = "" -- do not download in test system
                  | otherwise  = masalaDataDir </> "downloads"

--- The directory where the tar files of the sources of uploaded packages
--- are stored.
downloadTarDir :: String
downloadTarDir = "TARFILES"

------------------------------------------------------------------------------
-- Access to the CPM repository manager:

--- The URL to upload a package specification to CPM so that other
--- programmers can use them. The package specification must be provided
--- on stdin and the package's tar file must exist in the `downloadTarDir`.
cpmUploadURL :: String
cpmUploadURL =
  "https://www-ps.informatik.uni-kiel.de/~cpm/cpm-upload-masala.cgi"

--- The base directory where the data of CPM is stored.
cpmBaseDir :: String
cpmBaseDir | testSystem = "/home/mh/public_html/curry/cpm"
           | otherwise  = "/net/medoc/home/cpm/public_html"

--- Documentation URL of a package version in the CPM repository.
packageURLinCPM :: String -> String -> String
packageURLinCPM pkgname version =
  let pkgid = pkgname ++ if null version then "" else '-' : version
  in "https://cpm.curry-lang.org/pkgs/" ++ pkgid ++ ".html"

------------------------------------------------------------------------------
