--------------------------------------------------------------------------
--- This module implements the views related to the standard controllers
--- in a Spicey application.
--- In particular, it defines a default view for login
--- and a view of a list of user processes.
--------------------------------------------------------------------------

module View.Validation
  ( wValidation )
 where

import HTML.WUI
import HTML.Base
import System.Spicey

wValidation
  :: WuiSpec String -- LoginName or Email
wValidation =
  withRendering
   wRequiredString
   (renderLabels validationLabelList)

validationLabelList :: HTML h => [[h]]
validationLabelList =
    [[textstyle "spicey_label spicey_label_for_type_string" "LoginName"]]