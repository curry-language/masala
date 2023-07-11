------------------------------------------------------------------------------
--- This is a simple script to test the batch upload functionality.
---
--- Install this script by `pakcs :l UploadScript :save :q`.
------------------------------------------------------------------------------

import HTML.Base      ( string2urlencoded )
import System.Process ( system )
import System.URL

import Config.Masala         ( baseURL )
import System.Authentication ( getUserHash )

--- Gets the login name, password and package specification file name from
--- the user and call the upload script.
main :: IO ()
main = do
  putStr "Login name: "
  login <- getLine
  putStr "Password: "
  system "stty -echo"
  passwd <- getLine
  system "stty echo"
  putStr "\nPackage specification file (default: package.json): "
  pkgfile <- getLine
  pkgtxt <- readFile (if null pkgfile then "package.json" else pkgfile)
  cryptpasswd <- getUserHash login passwd
  callUploadBy login cryptpasswd pkgtxt

--- Calls the upload batch URL of Masala.
callUploadBy :: String -> String -> String -> IO ()
callUploadBy login passwd pkgtxt = do
  let upurl = baseURL ++ "?UploadBy/" ++
              string2urlencoded login ++ "/" ++
              string2urlencoded passwd ++ "/" ++
              string2urlencoded pkgtxt
  getContentsOfURL upurl >>= putStr

