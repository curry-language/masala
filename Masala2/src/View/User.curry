module View.User
  ( wUser, tuple2User, user2Tuple, wUserType, showUserView, listUserView )
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

--- The WUI specification for the entity type User.
--- It also includes fields for associated entities.
wUser
  :: [Package]
  -> [Package]
  -> WuiSpec (String
             ,String
             ,String
             ,String
             ,String
             ,String
             ,String
             ,Maybe ClockTime
             ,[Package]
             ,[Package])
wUser maintainerPackageList watchingPackageList =
  withRendering
   (w10Tuple wRequiredString wRequiredString wRequiredString wString
     wRequiredString
     wRequiredString
     wString
     (wUncheckMaybe (toClockTime (CalendarTime 2018 1 1 0 0 0 0)) wDateType)
     (wMultiCheckSelect (\package -> [htxt (packageToShortView package)])
       maintainerPackageList)
     (wMultiCheckSelect (\package -> [htxt (packageToShortView package)])
       watchingPackageList))
   (renderLabels userLabelList)

--- Transformation from data of a WUI form to entity type User.
tuple2User
  :: User
  -> (String
     ,String
     ,String
     ,String
     ,String
     ,String
     ,String
     ,Maybe ClockTime
     ,[Package]
     ,[Package])
  -> (User,[Package],[Package])
tuple2User
    userToUpdate
    (loginName,publicName,email,publicEmail,role,password,token,lastLogin
    ,maintainerPackages,watchingPackages) =
  (setUserLoginName
    (setUserPublicName
      (setUserEmail
        (setUserPublicEmail
          (setUserRole
            (setUserPassword
              (setUserToken (setUserLastLogin userToUpdate lastLogin) token)
              password)
            role)
          publicEmail)
        email)
      publicName)
    loginName
  ,maintainerPackages
  ,watchingPackages)

--- Transformation from entity type User to a tuple
--- which can be used in WUI specifications.
user2Tuple
  :: (User,[Package],[Package])
  -> (String
     ,String
     ,String
     ,String
     ,String
     ,String
     ,String
     ,Maybe ClockTime
     ,[Package]
     ,[Package])
user2Tuple (user,maintainerPackages,watchingPackages) =
  (userLoginName user
  ,userPublicName user
  ,userEmail user
  ,userPublicEmail user
  ,userRole user
  ,userPassword user
  ,userToken user
  ,userLastLogin user
  ,maintainerPackages
  ,watchingPackages)

--- WUI Type for editing or creating User entities.
--- Includes fields for associated entities.
wUserType
  :: User -> [Package] -> [Package] -> WuiSpec (User,[Package],[Package])
wUserType user maintainerPackageList watchingPackageList =
  transformWSpec (tuple2User user,user2Tuple)
   (wUser maintainerPackageList watchingPackageList)

--- Supplies a view to show the details of a User.
showUserView
  :: UserSessionInfo -> User -> [Package] -> [Package] -> [BaseHtml]
showUserView _ user maintainerPackages watchingPackages =
  userToDetailsView user maintainerPackages watchingPackages
   ++ [hrefPrimSmButton (listRoute user) [htxt "To User list"]]

--- Compares two User entities. This order is used in the list view.
leqUser :: User -> User -> Bool
leqUser x1 x2 =
  (userLoginName x1
  ,userPublicName x1
  ,userEmail x1
  ,userPublicEmail x1
  ,userRole x1)
   <= (userLoginName x2
      ,userPublicName x2
      ,userEmail x2
      ,userPublicEmail x2
      ,userRole x2)

--- Supplies a list view for a given list of User entities.
--- Shows also show/edit/delete buttons if the user is logged in.
--- The arguments are the session info and the list of User entities.
listUserView :: UserSessionInfo -> [User] -> [BaseHtml]
listUserView sinfo users =
  [h1 [htxt "User list"]
  ,spTable ([take 8 userLabelList] ++ map listUser (sortBy leqUser users))]
  where
    listUser user =
      userToListView user
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[hrefPrimBadge (showRoute user) [htxt "Show"]]
                   ,[hrefPrimBadge (editRoute user) [htxt "Edit"]]
                   ,[hrefPrimBadge (deleteRoute user) [htxt "Delete"]]])
