------------------------------------------------------------------------------
--- This is a simple script to test the batch upload functionality.
---
--- Install this script by `pakcs :l UploadScript :save :q`.
------------------------------------------------------------------------------

module UploadScript ( main ) where

import Network.URL        ( string2urlencoded )
import System.Environment ( getArgs )
import System.Process     ( system )
import System.URL         ( getContentsOfURL )

import Config.Masala         ( getMainScriptURL )
import System.Authentication ( getUserHash )

data Options = Options
  { optName    :: String
  , optPasswd  :: String
  , optPublish :: Bool
  , optForce   :: Bool
  }

defaultOptions :: Options
defaultOptions = Options "" "" True False

main :: IO ()
main = do
  args <- getArgs
  processArgs args defaultOptions

processArgs :: [String] -> Options -> IO ()
processArgs args opts = case args of
  "--name":name:margs       -> processArgs margs (opts { optName = name })
  "--password":passwd:margs -> processArgs margs (opts { optPasswd = passwd })
  "--nopublish":margs       -> processArgs margs (opts { optPublish = False })
  "--force":margs           -> processArgs margs (opts { optForce = True })
  [pkg]                     -> mainWithOpts opts pkg
  _ -> putStrLn $ "Illegal arguments: " ++ unwords args ++ "\n" ++ helpText

helpText :: String
helpText = unlines
  [ "Usage: UploadScript [options] <package specification file>"
  , ""
  , "--name <loginname>      provide login name (otherwise: ask)"
  , "--password <password>   provide password (otherwise: ask)"
  , "--nopublish             do not publish package (only upload to Masala)"
  , "--force                 overwrite existing package version (if allowed)"
  ]

--- Gets the login name, password and package specification file name from
--- the user and call the upload script.
mainWithOpts :: Options -> String -> IO ()
mainWithOpts opts0 pkgfile = do
  opts1 <- if null (optName opts0) then do putStr "Login name: "
                                           login <- getLine
                                           return opts0 { optName = login }
                                   else return opts0
  opts2 <- if null (optPasswd opts1) then do putStr "Password: "
                                             system "stty -echo"
                                             pass <- getLine
                                             system "stty echo"
                                             putChar '\n'
                                             return opts1 { optPasswd = pass }
                                   else return opts1
  cryptpasswd <- getUserHash (optName opts2) (optPasswd opts2)
  pkgtxt      <- readFile pkgfile
  callUploadBy (optName opts2) cryptpasswd (optPublish opts2) (optForce opts2)
               pkgtxt

--- Calls the upload batch URL of Masala.
callUploadBy :: String -> String -> Bool -> Bool -> String -> IO ()
callUploadBy login passwd publish force pkgtxt = do
  masalaurl <- getMainScriptURL
  let upurl = masalaurl ++ "?UploadBy/" ++
              string2urlencoded login ++ "/" ++
              string2urlencoded passwd ++ "/" ++
              string2urlencoded (show publish) ++ "/" ++
              string2urlencoded (show force) ++ "/" ++
              string2urlencoded pkgtxt
  getContentsOfURL upurl >>= putStrLn
