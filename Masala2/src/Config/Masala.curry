-------------------------------------------------------------------------------
-- Global configurations for the Masala system
-------------------------------------------------------------------------------

module Config.Masala
 where

import System.FilePath ( (</>) )

--- Is the current installation a test system?
--- In a test systems, mails are not really sent but its contents is
--- just shown in the web page which sent it (see `Controller.Mail`).
testSystem :: Bool
testSystem = True

--- Email address of administrator:
adminEmail :: String
adminEmail = "masala@curry-lang.org"

--- The name of the main script of the Masala system.
baseCGI :: String
baseCGI = "spicey.cgi"

--- The URL of the main script of the module system
--- (used to generate external URLs for modules and master programs):
baseURL :: String
baseURL =
  (if testSystem then "http://localhost/~mh/masala2"
                 else "https://www-ps.informatik.uni-kiel.de/~masala/masala2")
  ++ "/" ++ baseCGI

--- The system hash key used to encode passwords
--- (compare `System.Authentication`).
systemHashKey :: String
systemHashKey = "3masala25" -- change this key for every spicey instance

--- Documentation URL of a package in the CPM repository.
packageURLinCPM :: String -> String
packageURLinCPM pkgname =
  "https://cpm.curry-lang.org/pkgs/" ++ pkgname ++ ".html"

--- Log file for emails.
emailLogFile :: String
emailLogFile = "data" </> "EMAILLOG.txt"

--- The directory where the package specifications (`package.json`) are stored.
packageSpecDir :: String
packageSpecDir = "data" </> "packages"

--- The directory where the sources of uploaded packages are stored.
--- If it is empty, the sources are not downloaded.
downloadSourceDir :: String
downloadSourceDir = "" -- do not download
--downloadSourceDir = "data" </> "downloads"

--- The directory where the tar files of the sources of uploaded packages
--- are stored.
downloadTarDir :: String
downloadTarDir = "TARFILES"
