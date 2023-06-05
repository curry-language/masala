module View.Registration
  ( wRegistration ) where

import Data.List
import Data.Time
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.WUI
import Model.Masala2
import Config.EntityRoutes
import System.SessionInfo
import System.Spicey
import View.EntitiesToHtml

--- The WUI specification for the registration input.
wRegistration :: WuiSpec (String, String, String, String)
wRegistration =
  withRendering (w4Tuple wRequiredString wRequiredString wRequiredString wPassword)
    (renderLabels RegistrationLabelList)

wPassword :: WuiSpec String
wPassword = wRequiredString
  `withCondition` checkIfPasswordFine
  `withError` "Password not ok"

checkIfPasswordFine :: String -> Bool
checkIfPasswordFine = checkPasswordLength

checkPasswordLength :: String -> Bool
checkPasswordLength uncryptpasswd = length uncryptpasswd >= 8
