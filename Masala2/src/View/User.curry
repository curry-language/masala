module View.User
  ( wUser, tuple2User, user2Tuple
  , wUserEdit, tuple2UserEdit, user2TupleEdit, wUserEditType
  , wUserEditAdmin, tuple2UserEditAdmin, user2TupleEditAdmin, wUserEditTypeAdmin
  , wPasswordEdit
  , showUserView, showUserViewAdmin, showUserViewLess, listUserView )
where

import Data.List
import Data.Time
import HTML.Base
import HTML.Styles.Bootstrap4
import HTML.WUI
import Model.Masala2
import Config.EntityRoutes
import Config.Roles
import System.SessionInfo
import System.Spicey
import View.EntitiesToHtml
import View.Registration

--- The WUI specification for the entity type User.
wUserEdit
  :: WuiSpec (String -- LoginName
             ,String -- PublicName
             ,String -- Email
             ,String -- PublicEmail
             )
wUserEdit =
  withRendering
   (w4Tuple
    (wConstant stringToHtml) -- LoginName
    wRequiredString -- PublicName
    (wConstant stringToHtml) -- Email
    wString -- PublicEmail
   )
   (renderLabels userLabelList)

--- The WUI specification for the entity type User
--- when the logged in User is an Admin.
wUserEditAdmin
  :: String -- Current Role
  -> WuiSpec (String -- LoginName
             ,String -- PublicName
             ,String -- Email
             ,String -- PublicEmail
             ,String -- Role
             )
wUserEditAdmin role =
  withRendering
   (w5Tuple
    wRequiredString -- LoginName
    wRequiredString -- PublicName
    wRequiredString -- Email
    wString -- PublicEmail
    (wSelect id roles) -- Role
   )
   (renderLabels userLabelListAdmin)
  where
    roles = if role == roleInvalid
              then [roleInvalid]
              else [roleNotTrusted, roleTrusted, roleAdmin]

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

--- Transformation from data of a WUI form to entity type User
--- when editing an User.
tuple2UserEdit
  :: User
  -> (String, String, String, String)
  -> User
tuple2UserEdit user (_, publicName, _, publicEmail) =
  setUserPublicEmail 
    (setUserPublicName user publicName)
    publicEmail

--- Transformation from data of a WUI form to entity type User
--- when editing an User as an Admin.
tuple2UserEditAdmin
  :: User
  -> (String, String, String, String, String)
  -> User
tuple2UserEditAdmin user (_, publicName, _, publicEmail, role) =
  setUserRole 
    (setUserPublicEmail 
      (setUserPublicName user publicName)
      publicEmail)
    role

--- Transformation from entity type User to a tuple
--- which can be used in WUI specifications
--- when editing an User.
user2TupleEdit
  :: User
  -> (String, String, String, String)
user2TupleEdit user =
  (userLoginName user
  ,userPublicName user
  ,userEmail user
  ,userPublicEmail user)

--- Transformation from entity type User to a tuple
--- which can be used in WUI specifications
--- when editing an User as an Admin.
user2TupleEditAdmin
  :: User
  -> (String, String, String, String, String)
user2TupleEditAdmin user =
  (userLoginName user
  ,userPublicName user
  ,userEmail user
  ,userPublicEmail user
  ,userRole user)

--- WUI Type for editing User entities.
wUserEditType :: User -> WuiSpec User
wUserEditType user =
  transformWSpec (tuple2UserEdit user, user2TupleEdit) wUserEdit

--- WUI Type for editing User entities as an Admin.
wUserEditTypeAdmin :: User -> WuiSpec User
wUserEditTypeAdmin user =
  transformWSpec
    (tuple2UserEditAdmin user, user2TupleEditAdmin)
    (wUserEditAdmin (userRole user))

--- The WUI specification for editing the password of an User.
--- User, old password, new password, new password (again)
wPasswordEdit :: WuiSpec (User,String,String,String)
wPasswordEdit =
  w4Tuple wHidden wMasalaPassword wMasalaPassword wMasalaPassword
    `withCondition` (\ (_,_,newp1,newp2) -> newp1 == newp2)
    `withError` "The new passwords must be equal"
    `withRendering` (renderLabels passwordEditLabelList)

--- Supplies a view to show the details of a User.
showUserView
  :: UserSessionInfo -> User -> [BaseHtml]
showUserView _ user =
  userToDetailsView user

--- Supplies a view to show the details of a User
--- when not logged in as that User.
showUserViewLess
  :: UserSessionInfo -> User -> [BaseHtml]
showUserViewLess _ user =
  userToDetailsViewLess user ++
  [ hrefPrimSmButton (showRoute user ++ "/Maintaining") [htxt "Maintained Packages"]]

--- Supplies a view to show the details of a User for the admin.
showUserViewAdmin
  :: UserSessionInfo -> User -> [BaseHtml]
showUserViewAdmin _ user =
  userToDetailsViewAdmin user ++
  [ hrefPrimSmButton (editRoute user) [htxt "Change data"], nbsp
  , hrefPrimSmButton (editRoute user ++ "/Password") [htxt "Change Password"], nbsp
  , hrefPrimSmButton (showRoute user ++ "/Maintaining") [htxt "Maintained Packages"], nbsp
  , hrefPrimSmButton (showRoute user ++ "/Watching") [htxt "Watched Packages"]]

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
  ,spTable ([take 8 userLabelListAdmin] ++ map listUser (sortBy leqUser users))]
  where
    listUser user =
      userToListView user
       ++ (if userLoginOfSession sinfo == Nothing
              then []
              else [[hrefPrimBadge (showRoute user) [htxt "Show"]]
                   ,[hrefPrimBadge (editRoute user) [htxt "Edit"]]
                   ,[hrefPrimBadge (deleteRoute user) [htxt "Delete"]]])
