module View.Registration
  ( wRegistration, wMasalaPassword ) where

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
wRegistration :: WuiSpec (String, String, String, String, String)
wRegistration =
  withRendering
    wRegistrationSpec
    (renderLabels registrationLabelList)
  where
    wRegistrationSpec :: WuiSpec (String, String, String, String, String)
    wRegistrationSpec =
      w5Tuple wRequiredString wRequiredString wMasalaEmail
              wMasalaPassword wMasalaPassword
      `withCondition`  (\ (_,_,_,newp1,newp2) -> newp1 == newp2)
      `withError` "The passwords must be equal"

--- A WUI field for a Masala Email.
--- String is required and the syntax must be correct.
wMasalaEmail :: WuiSpec String
wMasalaEmail = wRequiredString
  `withCondition` checkEmailSyntax
  `withError` "The email must be a correct email address"

--- Checks for a string that it is an email address.
checkEmailSyntax :: String -> Bool
checkEmailSyntax s = '@' `elem` s

--- A WUI field for a Masala password.
--- Must be a password and long enough.
wMasalaPassword :: WuiSpec String
wMasalaPassword = 
  wPassword
    `withCondition` checkIfPasswordFine
    `withError` "The password must be at least 8 symbols long"
  where
    checkIfPasswordFine :: String -> Bool
    checkIfPasswordFine = checkPasswordLength

    checkPasswordLength :: String -> Bool
    checkPasswordLength uncryptpasswd = length uncryptpasswd >= 8
