module View.ValidationToken
  ( wValidationToken, tuple2ValidationToken, validationToken2Tuple
  , wValidationTokenType, showValidationTokenView, listValidationTokenView )
where

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

--- The WUI specification for the entity type ValidationToken.
--- It also includes fields for associated entities.
wValidationToken :: [User] -> WuiSpec (String,ClockTime,User)
wValidationToken userList =
  withRendering
   (wTriple wRequiredString wDateType (wSelect userToShortView userList))
   (renderLabels validationTokenLabelList)

--- Transformation from data of a WUI form to entity type ValidationToken.
tuple2ValidationToken
  :: ValidationToken -> (String,ClockTime,User) -> ValidationToken
tuple2ValidationToken validationTokenToUpdate (token,validSince,user) =
  setValidationTokenToken
   (setValidationTokenValidSince
     (setValidationTokenUserValidatingKey validationTokenToUpdate
       (userKey user))
     validSince)
   token

--- Transformation from entity type ValidationToken to a tuple
--- which can be used in WUI specifications.
validationToken2Tuple :: User -> ValidationToken -> (String,ClockTime,User)
validationToken2Tuple user validationToken =
  (validationTokenToken validationToken
  ,validationTokenValidSince validationToken
  ,user)

--- WUI Type for editing or creating ValidationToken entities.
--- Includes fields for associated entities.
wValidationTokenType
  :: ValidationToken -> User -> [User] -> WuiSpec ValidationToken
wValidationTokenType validationToken user userList =
  transformWSpec
   (tuple2ValidationToken validationToken,validationToken2Tuple user)
   (wValidationToken userList)

--- Supplies a view to show the details of a ValidationToken.
showValidationTokenView
  :: UserSessionInfo -> ValidationToken -> User -> [BaseHtml]
showValidationTokenView _ validationToken relatedUser =
  validationTokenToDetailsView validationToken relatedUser
   ++ [hrefPrimSmButton (listRoute validationToken)
        [htxt "To ValidationToken list"]]

--- Compares two ValidationToken entities. This order is used in the list view.
leqValidationToken :: ValidationToken -> ValidationToken -> Bool
leqValidationToken x1 x2 =
  (validationTokenToken x1,validationTokenValidSince x1)
   <= (validationTokenToken x2,validationTokenValidSince x2)

--- Supplies a list view for a given list of ValidationToken entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of ValidationToken entities.
listValidationTokenView :: UserSessionInfo -> [ValidationToken] -> [BaseHtml]
listValidationTokenView sinfo validationTokens =
  [h1 [htxt "ValidationToken list"]
  ,spTable
    ([take 2 validationTokenLabelList]
      ++ map listValidationToken
          (sortBy leqValidationToken validationTokens))]
  where
    listValidationToken validationToken =
      validationTokenToListView validationToken
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[hrefPrimBadge (showRoute validationToken) [htxt "Show"]]
                   ,[hrefPrimBadge (editRoute validationToken) [htxt "Edit"]]
                   ,[hrefPrimBadge (deleteRoute validationToken)
                      [htxt "Delete"]]])
