-------------------------------------------------------------------------------
-- Global configurations for the Masala system
-------------------------------------------------------------------------------

module Config.Masala
 where

-- Email address of administrator:
adminEmail :: String
adminEmail = "masala@curry-lang.org"

-- The name of the main script of the Masala system.
baseCGI :: String
baseCGI = "spicey.cgi"

-- The URL of the main script of the module system
-- (used to generate external URLs for modules and master programs):
baseURL :: String
--baseURL = "https://masala.curry-lang.org/" ++ baseCGI
baseURL = "http://localhost/~mh/masala2/" ++ baseCGI -- for testing

--- The system hash key used to encode passwords
--- (compare `System.Authentication`).
systemHashKey :: String
systemHashKey = "3masala25" -- change this key for every spicey instance

