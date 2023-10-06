module Config.Roles where

roleInvalid :: String
roleInvalid = "Invalid"

roleNotTrusted :: String
roleNotTrusted = "Not Trusted"

roleTrusted :: String 
roleTrusted = "Trusted"

roleAdmin :: String 
roleAdmin = "Admin"

--- Shows a rule in human-readable form
showRole :: String -> String
showRole r
  | r == roleAdmin      = "administrator"
  | r == roleTrusted    = "trusted user"
  | r == roleNotTrusted = "not yet trusted user"
  | r == roleInvalid    = "not yet valided user"
  | otherwise           = "unknown role"
  