module View.Registration
  ( wRegistration, wMasalaPassword, wPasswords ) where

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

{-
wRegistrationSpec' :: WuiSpec (String, String, String, String, String)
wRegistrationSpec' = w5Tuple wRequiredString wRequiredString wMasalaEmail wMasalaPassword wMasalaPassword
  `withCondition` checkIfPasswordsEqual -- (\_ -> False)
  `withError` "The passwords must be equal"
-}

wRegistrationSpec :: WuiSpec (String, String, String, String, String)
wRegistrationSpec =
  transformWSpec (\ ((a,b,c),(d,e)) -> (a,b,c,d,e),
                  \ (a,b,c,d,e) -> ((a,b,c),(d,e)))
    (wJoinTuple wLoginNameEmail wPasswords)

wLoginNameEmail :: WuiSpec (String, String, String)
wLoginNameEmail = wTriple wRequiredString wRequiredString wMasalaEmail

wPasswords :: WuiSpec (String, String)
wPasswords = wPair wMasalaPassword wMasalaPassword
  `withCondition` (\ (p1,p2) -> p1 == p2)
  `withError` "The passwords must be equal"
  `withRendering`
    (\hes -> inline (intersperse (textstyle "spicey_label" "Repeat password:")
                                 hes))

wMasalaEmail :: WuiSpec String
wMasalaEmail = wRequiredString
  `withCondition` checkEmailSyntax
  `withError` "The email must be a correct email address"

checkEmailSyntax :: String -> Bool
checkEmailSyntax s = '@' `elem` s

wMasalaPassword :: WuiSpec String
wMasalaPassword = wPassword
  `withCondition` checkIfPasswordFine
  `withError` "The password must be at least 8 symbols long"

checkIfPasswordFine :: String -> Bool
checkIfPasswordFine = checkPasswordLength

checkPasswordLength :: String -> Bool
checkPasswordLength uncryptpasswd = length uncryptpasswd >= 8

checkIfPasswordsEqual :: (String, String, String, String, String) -> Bool
checkIfPasswordsEqual (_, _, _, password1, password2) = password1 == password2
