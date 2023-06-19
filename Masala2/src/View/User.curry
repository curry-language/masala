module View.User
  ( wUser, tuple2User, user2Tuple, wUserType
  , wUserEdit, tuple2UserEdit, user2TupleEdit, wUserTypeEdit
  , wPasswordEdit
  , showUserView, listUserView )
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
import View.Registration

--- The WUI specification for the entity type User.
--- It also includes fields for associated entities.
wUser
  :: [Package]
  -> [Package]
  -> WuiSpec (String -- LoginName
             ,String -- PublicName
             ,String -- Email
             ,String -- PublicEmail
             ,String -- Role
             ,String -- Password
             ,String -- Token
             ,Maybe ClockTime -- LastLogin
             ,[Package] -- Maintains
             ,[Package] -- Watching
             )
wUser maintainerPackageList watchingPackageList =
  withRendering
   (w10Tuple
    (wConstant stringToHtml) -- LoginName
    wRequiredString -- PublicName
    (wConstant stringToHtml) -- Email
    wString -- PublicEmail
    wHidden -- Role
    wHidden -- Password
    wHidden -- Token
    wHidden -- LastLogin
    --(wUncheckMaybe (toClockTime (CalendarTime 2018 1 1 0 0 0 0)) wDateType) -- LastLogin
    (wMultiCheckSelect (\package -> [htxt (packageToShortView package)])
      maintainerPackageList) -- Maintains
    (wMultiCheckSelect (\package -> [htxt (packageToShortView package)])
      watchingPackageList) -- Watching
   )
   (renderLabels userLabelList)

wUserEdit
  :: [Package]
  -> [Package]
  -> WuiSpec ( String
             , String
             , String
             , String
             , [Package]
             , [Package]
             )
wUserEdit maintainerPackageList watchingPackageList = 
  withRendering
    (w6Tuple
      (wConstant stringToHtml)  -- LoginName
      wRequiredString           -- PublicName
      (wConstant stringToHtml)  -- Email
      wString                   -- PublicEmail
      (wMultiCheckSelect (\package -> [htxt (packageToShortView package)])
        maintainerPackageList) -- Maintains
      (wMultiCheckSelect (\package -> [htxt (packageToShortView package)])
        watchingPackageList) -- Watching
    )
    (renderLabels userEditLabelList)

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

tuple2UserEdit
  :: User
  -> (String
     ,String
     ,String
     ,String
     ,[Package]
     ,[Package])
  -> (User,[Package],[Package])
tuple2UserEdit
    userToUpdate
    (loginName,publicName,email,publicEmail,maintainerPackages,watchingPackages) =
  (setUserLoginName
    (setUserPublicName
      (setUserEmail
        (setUserPublicEmail
          userToUpdate
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

user2TupleEdit
  :: (User,[Package],[Package])
  -> (String
     ,String
     ,String
     ,String
     ,[Package]
     ,[Package])
user2TupleEdit (user,maintainerPackages,watchingPackages) =
  (userLoginName user
  ,userPublicName user
  ,userEmail user
  ,userPublicEmail user
  ,maintainerPackages
  ,watchingPackages)

--- WUI Type for editing or creating User entities.
--- Includes fields for associated entities.
wUserType
  :: User -> [Package] -> [Package] -> WuiSpec (User,[Package],[Package])
wUserType user maintainerPackageList watchingPackageList =
  transformWSpec (tuple2User user,user2Tuple)
   (wUser maintainerPackageList watchingPackageList)

wUserTypeEdit
  :: User -> [Package] -> [Package] -> WuiSpec (User,[Package],[Package])
wUserTypeEdit user maintainerPackageList watchingPackageList =
  transformWSpec (tuple2UserEdit user,user2TupleEdit)
    (wUserEdit maintainerPackageList watchingPackageList)

wPasswordEdit :: WuiSpec (User,String,String,String)
wPasswordEdit =
  withRendering
      (transformWSpec (\ ((a,b),(c,d)) -> (a,b,c,d),
                       \ (a,b,c,d) -> ((a,b),(c,d)))
          (wJoinTuple (wPair wHidden wMasalaPassword) wPasswords))
      (renderLabels passwordEditLabelList)

--- Supplies a view to show the details of a User.
showUserView
  :: UserSessionInfo -> User -> [Package] -> [Package] -> [BaseHtml]
showUserView _ user maintainerPackages watchingPackages =
  --userToDetailsView user maintainerPackages watchingPackages
  userToDetailsViewLess user maintainerPackages watchingPackages
   ++ [hrefPrimSmButton (editRoute user ++ "/Password") [htxt "Change Password"]]
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
